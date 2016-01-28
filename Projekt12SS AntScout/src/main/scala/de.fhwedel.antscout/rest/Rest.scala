package de.fhwedel.antscout
package rest

import akka.pattern.ask
import akka.util.duration._
import net.liftweb.http.rest.RestHelper
import net.liftweb.json.JsonDSL._
import osm.OsmMap
import dijkstra.DijkstraService
import routing.RoutingService
import antnet.JamGen
import antnet.{ AntNode, AntWay, AntMap }
import akka.dispatch.Await
import akka.util.Timeout
import net.liftweb.json.JsonAST.JArray
import net.liftweb.common.{ Empty, Full, Box, Logger }
import net.liftweb.http.{ NamedCometListener, S, LiftSession }

/**
 * Rest-Schnittstelle für Knoten, Wege und Pfade.
 */
object Rest extends Logger with RestHelper {

  implicit val timeout = Timeout(10 seconds)

  serve {
    // Anfrage nach den Daten eines bestimmten Knotens
    case Get(List("node", id), _) =>
      Node(Full(id))
      // Eingehende Wege
      val incomingWays = AntMap.incomingWays.find {
        case (node, ways) => node.id == id
      } map {
        case (node, ways) => ways
      } getOrElse Set[AntWay]()
      // Ausgehende Wege
      val outgoingWays = AntMap.outgoingWays.find {
        case (node, ways) => node.id == id
      } map {
        case (node, ways) => ways
      } getOrElse Set[AntWay]()
      ("incomingWays" -> incomingWays.map(_.toJson)) ~
        ("outgoingWays" -> outgoingWays.map(_.toJson))
    // Anfrage nach Knoten
    case Get(List("nodes"), _) =>
      AntMap.nodes.map(node => OsmMap.nodes(node.id).toJson): JArray
    // Anfrage nach einem Pfad
    case Get(List("path", source, destination), _) =>
      // Quelle in die Session schreiben
      Source(Full(source))
      // Ziel in die Session schreiben
      Destination(Full(destination))
      // Anfrage an Dijkstra nach dem Pfad 
      if (Settings.Dji) {
        //info("Neue Endpunkte: Dijsktra ausführen - Pfad suchen von Knoten %s nach Knoten %s".format(source, destination))
        val pathFuture = (system.actorFor(Iterable("user", AntScout.ActorName, DijkstraService.ActorName)) ?
          DijkstraService.FindPath(source, destination))
        if (Settings.NoPath) {
          Settings.NoPath = false
          system.actorFor(Iterable("user", AntScout.ActorName, JamGen.ActorName)) !
            JamGen.SetUp()
        }
        for {
          // Auf die Antwort von Dijkstra warten
          path <- Await.result(pathFuture, 5 seconds).asInstanceOf[Box[Seq[AntWay]]] ?~ "No path found" ~> 404

        } yield {
          // Json aus den Daten erzeugen
          val (length, tripTime) = path.foldLeft(0.0, 0.0) {
            case ((lengthAcc, tripTimeAcc), way) => (way.length + lengthAcc, way.tripTime + tripTimeAcc)
          }
          //info("Neue Endpunkte: Dijsktra erfolgreich ausgeführt - Pfad gefunden von Knoten %s nach Knoten %s: %s".format(source, destination, path))
          //info("Neue Endpunkte: neue Pfadlänge: %s Meter, neue Fahrtzeit: %s Minuten".format(length, tripTime.asInstanceOf[Double] / 60))
          ("length" -> "%.4f".format(length / 1000)) ~
            ("lengths" ->
              JArray(List(("unit" -> "m") ~
                ("value" -> "%.4f".format(length))))) ~
              ("tripTime" -> "%.4f".format(tripTime / 60)) ~
              ("tripTimes" ->
                JArray(List(
                  ("unit" -> "s") ~
                    ("value" -> "%.4f".format(tripTime)),
                  ("unit" -> "h") ~
                    ("value" -> "%.4f".format(tripTime / 3600))))) ~
                ("ways" -> path.map(_.toJson))
        }
      } else {
        //info("Neue Endpunkte: RoutingService ausführen - Pfad suchen von Knoten %s nach Knoten %s".format(source, destination))
        // Anfrage an den RoutingService nach dem Pfad 
        val pathFuture = (system.actorFor(Iterable("user", AntScout.ActorName, RoutingService.ActorName)) ?
          RoutingService.FindPath(AntNode(source), AntNode(destination)))
        if (Settings.NoPath) {
          Settings.NoPath = false
          system.actorFor(Iterable("user", AntScout.ActorName, JamGen.ActorName)) !
            JamGen.SetUp()
        }
        for {
          // Auf die Antwort vom RoutingService warten
          path <- Await.result(pathFuture, 5 seconds).asInstanceOf[Box[Seq[AntWay]]] ?~ "No path found" ~> 404
        } yield {
          if (Settings.NoPath) {
            Settings.NoPath = false
            system.actorFor(Iterable("user", AntScout.ActorName, JamGen.ActorName)) !
              JamGen.SetUp()
          }
          // Json aus den Daten erzeugen
          val (length, tripTime) = path.foldLeft(0.0, 0.0) {
            case ((lengthAcc, tripTimeAcc), way) => (way.length + lengthAcc, way.tripTime + tripTimeAcc)
          }
          //info("Neue Endpunkte: RoutingService erfolgreich ausgeführt - Pfad gefunden von Knoten %s nach Knoten %s: %s".format(source, destination, path))
          //info("Neue Endpunkte: neue Pfadlänge: %s Meter, neue Fahrtzeit: %s Minuten".format(length, tripTime.asInstanceOf[Double] / 60))
          ("length" -> "%.4f".format(length / 1000)) ~
            ("lengths" ->
              JArray(List(("unit" -> "m") ~
                ("value" -> "%.4f".format(length))))) ~
              ("tripTime" -> "%.4f".format(tripTime / 60)) ~
              ("tripTimes" ->
                JArray(List(
                  ("unit" -> "s") ~
                    ("value" -> "%.4f".format(tripTime)),
                  ("unit" -> "h") ~
                    ("value" -> "%.4f".format(tripTime / 3600))))) ~
                ("ways" -> path.map(_.toJson))
        }
      }
    // Anfrage nach den OSM-Knoten
    case Get(List("osmnodes"), _) => {
      OsmMap.nodes.values.map(_.toJson): JArray
    }
    // Anfrage nach den Wegen
    case Get(List("ways"), _) => {
      AntMap.ways.map(_.toJson): JArray
    }
    // Anfrage nach den Daten eines bestimmten Weges
    case Get(List("way", id), _) => {
      //info("Daten von Weg %s abrufen".format(id))
      AntMap.ways.find(_.id == id).map(_.toJson)
    }
    // Weg-Änderungs-Anfrage
    case JsonPut(List("way", id), json -> _) =>
      // Weg suchen, der aktualisiert werden soll
      val way = AntMap.ways.find(_.id == id)
      // Aktualisierte Daten extrahieren
      val wayUpdate = json.extract[AntWay.Update]
      way.map { way =>
        // Aktualisierung durchführen
        way.update(wayUpdate)
        // Warten, bis die Aktualisierung durchgeführt wurde        
        //info("Wegänderung: Weg %s wurde geändert, neue Geschwindigkeit ist %s Kilometer pro Stunde".format(way.id, way.maxSpeed(true) * 3.6))
        // Pfad aktualisieren, falls notwendig
        for {
          path <- Path
        } yield {
          //info("Wegänderung: Pfad gefunden: %s".format(path))
          // Ist der aktualisierte Weg Teil des aktuellen Pfades?
          if (path.contains(way)) {
            //info("Wegänderung: Pfad muss angepasst werden")
            // Weg im Pfad ersetzen
            val newPath = (path.takeWhile(_ != way) :+ way) ++ path.dropWhile(_ != way).tail
            // Neuen Pfad in die Session schreiben
            Path(Full(newPath))
            // Neuen Pfad an den User-Interface-Aktor senden
            NamedCometListener.getDispatchersFor(Full("userInterface")) foreach { actor =>
              actor.map(_ ! RoutingService.Path(Full(newPath)))
            }
            val (length, tripTime) = path.foldLeft(0.0, 0.0) {
              case ((lengthAcc, tripTimeAcc), way) => (way.length + lengthAcc, way.tripTime + tripTimeAcc)
            }
            //info("Wegänderung: Pfad erfolgreich angepasst")
            //info("Wegänderung: neue Pfadlänge: %s Meter, neue Fahrtzeit: %s Minuten".format(length, tripTime / 60))
          }
          if (Settings.Dji) {
            val actSource = Source.get.get
            val actDestination = Destination.get.get
            val pathFuture = (system.actorFor(Iterable("user", AntScout.ActorName, DijkstraService.ActorName)) ?
              DijkstraService.FindPath(actSource, actDestination))
            for {
              // Auf die Antwort vom Dijkstra warten
              path <- Await.result(pathFuture, 5 seconds).asInstanceOf[Box[Seq[AntWay]]] ?~ "No path found" ~> 404
            } yield {
              // Neuen Pfad in die Session schreiben
              Path(Full(path))
              // Neuen Pfad an den User-Interface-Aktor senden
              NamedCometListener.getDispatchersFor(Full("userInterface")) foreach { actor =>
                actor.map(_ ! DijkstraService.Path(Full(path)))
              }
              val (length, tripTime) = path.foldLeft(0.0, 0.0) {
                case ((lengthAcc, tripTimeAcc), way) => (way.length + lengthAcc, way.tripTime + tripTimeAcc)
              }
              //info("Wegänderung: Dijsktra ausgeführt - Pfad gefunden von Knoten %s nach Knoten %s: %s".format(actSource, actDestination, path))
              //info("Wegänderung: neue Pfadlänge: %s Meter, neue Fahrtzeit: %s Minuten".format(length, tripTime.asInstanceOf[Double] / 60))

              //              // Json aus den Daten erzeugen
              //              val (length, tripTime) = path.foldLeft(0.0, 0.0) {
              //                case ((lengthAcc, tripTimeAcc), way) => (way.length + lengthAcc, way.tripTime + tripTimeAcc)
              //              }
              //              ("length" -> "%.4f".format(length / 1000)) ~
              //                ("lengths" ->
              //                  JArray(List(("unit" -> "m") ~
              //                    ("value" -> "%.4f".format(length))))) ~
              //                  ("tripTime" -> "%.4f".format(tripTime / 60)) ~
              //                  ("tripTimes" ->
              //                    JArray(List(
              //                      ("unit" -> "s") ~
              //                        ("value" -> "%.4f".format(tripTime)),
              //                      ("unit" -> "h") ~
              //                        ("value" -> "%.4f".format(tripTime / 3600))))) ~
              //                    ("ways" -> path.map(_.toJson))
            }
          }
        }
        // Aktualisierten Weg zurückgeben
        way.toJson
      }
  }
}
