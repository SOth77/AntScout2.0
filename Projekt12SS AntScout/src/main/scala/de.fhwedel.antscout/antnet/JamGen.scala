package de.fhwedel.antscout
package antnet

import akka.actor.{ ActorRef, ActorLogging, Actor }
import java.util.Random
import net.liftweb.http.{ NamedCometListener, S, LiftSession }
import net.liftweb.json.JsonAST.JArray
import net.liftweb.common.{ Empty, Full, Box, Logger }
import net.liftweb.http.rest.RestHelper
import net.liftweb.json.JsonDSL._
import dijkstra.DijkstraService
import akka.dispatch.Await
import akka.pattern.ask
import akka.util.duration._
import akka.util.Timeout

class JamGen extends Actor with ActorLogging {
  import JamGen._
  /**
   * Lift-Session
   */
  var liftSession: Option[LiftSession] = None

  implicit val timeout = Timeout(10 seconds)

  /**
   * Initialisiert den Dijkstra-Service.
   */
  def init() {
    log.info("Initialized")
  }

  /**
   * Callback-Funktion, die vor dem Start des Aktors ausgeführt wird.
   */
  override def preStart() {
    log.info("Initializing")
  }

  protected def receive = {
    //Stau erzeugen
    case StartGen() =>
      val rand = new Random()
      val iter = AntMap.ways.iterator
      val wayNumber = rand.nextInt(AntMap.ways.size)
      var changeNumber = Settings.Factor * (rand.nextInt(Settings.MaxChange) + 1)
      if (Settings.Positive) {
        if (rand.nextBoolean()) {
          changeNumber = 1 / changeNumber
        }
      }
      var i = 0
      var actElem = iter.next()
      while (i < wayNumber) {
        i = i + 1
        actElem = iter.next()
      }
      val speed = actElem.maxSpeed(true) * changeNumber     
      if (speed >= 1.0) {      
        actElem.maxSpeed_=(speed)
        actElem.maxSpeed(true)
        log.info("--------------------------------------------------------------------------------------------------------------------------------------------") 
        log.info("Durchschnittsgeschwindigkeit von Weg %s beträgt %s Meter pro Sekunde nach Anwendung des Staufaktors %s,".format(actElem.id, speed.toString, changeNumber.toString)) 
        if (Settings.Dji) {
          //Besten Weg nach Stau bestimmen
          for {
            liftSession <- liftSession
          } yield {
            S.initIfUninitted(liftSession) {
              val actSource = Source.get.openOr("empty")
              val actDestination = Destination.get.openOr("empty")
              log.info("Pfad von Knoten %s nach Knoten %s".format(actSource, actDestination)) 
              if (actSource != "empty" && actDestination != "empty") {
                val pathFuture = (system.actorFor(Iterable("user", AntScout.ActorName, DijkstraService.ActorName)) ?
                  DijkstraService.FindPath(actSource, actDestination))
                for {
                  // Auf die Antwort vom Dijkstra warten
                  path <- Await.result(pathFuture, 5 seconds).asInstanceOf[Box[Seq[AntWay]]] ?~ "No path found" ~> 404
                } yield {
                  // Neuen Pfad an den User-Interface-Aktor senden
                  NamedCometListener.getDispatchersFor(Full("userInterface")) foreach { actor =>
                    actor.map(_ ! DijkstraService.Path(Full(path)))
                    Path(Full(path))
                    // Json aus den Daten erzeugen
                    val (length, tripTime) = path.foldLeft(0.0, 0.0) {
                      case ((lengthAcc, tripTimeAcc), way) => (way.length + lengthAcc, way.tripTime + tripTimeAcc)
                    }
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
              }
            }
          }
        }
      }
    // }
    // Initialisierung
    case Initialize =>
      init()
    // Lift-Session
    case liftSession: LiftSession =>
      this.liftSession = Some(liftSession)
    case m: Any =>
      log.warning("Unknown message: %s" format m.toString)
  }
}

/**
 *  JamGen-Factory
 */
object JamGen {
  /**
   * Aktor-Name
   */
  val ActorName = "jamgen"

  /**
   * Starten der Stausimulation
   *
   * @param value Stauwert
   */
  case class StartGen()

  /**
   * Initialisierung
   */
  case object Initialize
}