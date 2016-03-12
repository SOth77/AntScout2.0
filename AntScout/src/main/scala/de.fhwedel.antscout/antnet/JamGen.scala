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
import de.fhwedel.antscout
import scala.io.Source
import akka.util._
import collection.mutable
import akka.actor.Cancellable
import java.util.concurrent.TimeUnit
import java.io._

/**
 * Koordiniert die Stauerzeugung und die Ausgabe
 */
class JamGen extends Actor with ActorLogging {
  import JamGen._

  /**
   * Lift-Session
   */
  var liftSession: Option[LiftSession] = None

  implicit val timeout = Timeout(10 seconds)

  /**
   * Cancellabeles werden beim Erzeugen von Schedulern zurückgegeben und erlauben es diese zu stoppen.
   */
  val cancellables = mutable.Set[Cancellable]()

  /**
   * Initialisiert den JamGen.
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

  /**
   * Ein Stau wird per Zufall erzeugt
   */
  def startgen() {
    // Staus werden nur erzeugt, wenn die maximale Ausgabezahl noch nicht erreicht ist
    if (Settings.ActOutput <= Settings.MaxOutput) {
      val rand = new Random()
      //Staukante zufällig bestimmen
      var wayNumber = rand.nextInt(AntMap.ways.size - 1) + 1
      var actElem = AntMap.ways.find(_.id == wayNumber.toString).get
      //Bei Beschränkung des Staubereichs im Bereich der besten Route neue Staukante zufällig bestimmen 
      if (!Settings.TrueRand) {
        for {
          liftSession <- liftSession
        } yield {
          S.initIfUninitted(liftSession) {
            for {
              path <- Path
            } yield {
              wayNumber = rand.nextInt(path.length)
              actElem = path.apply(wayNumber)
            }
          }
        }
      }
      //Staustärke und Ausprägung zufällig bestimmen
      var changeNumber = Settings.Factor * (rand.nextInt(Settings.MaxChange) + 1)
      if (Settings.Positive && rand.nextBoolean()) {
        changeNumber = 1 / changeNumber
      }
      val speed = actElem.maxSpeed(true) * changeNumber * 3.6
      if (speed >= 1.0) {
        log.info("Stauerzeugung - Zufall: Geschwindigkeit von %s Kilometer pro Stunde für Knoten %s".format(speed, actElem.id))
        //Kantenänderung an FrontEnd senden
        NamedCometListener.getDispatchersFor(Full("userInterface")) foreach { actor =>
          actor.map(_ ! Way(Full(actElem), Full(speed)))
        }
        //Verkehrsänderung in Staudatei speichern
        if (Settings.SaveJam != "empty") {
          val fw = new FileWriter(Settings.SaveJam, true)
          fw.write(actElem.id + "," + speed.toString + System.getProperty("line.separator"))
          fw.close();
        }
      }
    }
  }

  /**
   * Aktuell am besten bewerteten Pfad ausgeben
   */
  def pathOutput {
    // Ausgaben werden nur erzeugt, wenn die maximale Ausgabezahl noch nicht erreicht ist
    if (Settings.ActOutput <= Settings.MaxOutput) {
      for {
        liftSession <- liftSession
      } yield {
        S.initIfUninitted(liftSession) {
          for {
            path <- Path
          } yield {
            // Länge und Fahrzeit für den aktuell am besten bewerteten Pfad bestimmen
            val (length, tripTime) = path.foldLeft(0.0, 0.0) {
              case ((lengthAcc, tripTimeAcc), way) => (way.length + lengthAcc, way.tripTime + tripTimeAcc)
            }
            //Ausgabedatei abhängig vom verwendeten Algorithmus bestimmen
            var string = "empty"
            if (Settings.Dji && Settings.SaveDijkstra != "empty") {
              string = Settings.SaveDijkstra
            } else if (Settings.Dji == false && Settings.SaveAnt != "empty") {
              string = Settings.SaveAnt
            }
            val fw = new FileWriter(string, true)
            fw.write((tripTime / 60).toString + System.getProperty("line.separator"))
            fw.close();
            Settings.ActOutput = Settings.ActOutput + 1
          }
        }
      }
    }
  }

  /**
   * Ein Stau wird per Staudatei erzeugt
   */
  def readJam() {
    for {
      liftSession <- liftSession
    } yield {
      S.initIfUninitted(liftSession) {
        for {
          // Dateizeileniterator aus Sessionvariable laden
          lines <- antscout.Lines
        } yield {
          if (lines.hasNext) {
            val args = lines.next.split(",")
            val actElem = AntMap.ways.find(_.id == args(0)).get
            val speed = args(1).toDouble
            log.info("Stauerzeugung - Laden: Geschwindigkeit  %s Kilometer pro Stunde für Weg %s".format(speed, actElem.id))
            //Kantenänderung an FrontEnd senden
            NamedCometListener.getDispatchersFor(Full("userInterface")) foreach { actor =>
              actor.map(_ ! Way(Full(actElem), Full(speed)))
            }
            antscout.Lines(Full(lines))
          }
        }
      }
    }
  }

  // Erzeugen der Schedules für Stau und Ausgabe
  def setUp {
    if (Settings.Jamgen) {
      if (Settings.LoadJam == "empty") {
        //Schedule für zufallsbasierte Stauerzeugung starten  
        cancellables += context.system.scheduler.schedule(Duration.Zero, Duration(Settings.Frequency,
          TimeUnit.MILLISECONDS), self, StartGen())
      } else {
        for {
          liftSession <- liftSession
        } yield {
          S.initIfUninitted(liftSession) {
            //Schedule für vordefinierte Stauerzeugung starten       
            val file = Source.fromFile(Settings.LoadJam)
            antscout.Lines(Full(file.getLines()))
            cancellables += context.system.scheduler.schedule(Duration.Zero, Duration(Settings.Frequency,
              TimeUnit.MILLISECONDS), self, ReadJam())
          }
        }
      }
    }
    // Schedule für die Ausgabe starten
    var string = "empty"
    if (Settings.Dji && Settings.SaveDijkstra != "empty") {
      string = Settings.SaveDijkstra
    } else if (Settings.Dji == false && Settings.SaveAnt != "empty") {
      string = Settings.SaveAnt
    }
    if (string != "empty") {
      val fw = new FileWriter(string, true)
      fw.write("negative Staus erlaubt=" + Settings.Positive.toString + System.getProperty("line.separator"))
      fw.write("ungelenkter Zufall=" + Settings.TrueRand.toString + System.getProperty("line.separator"))
      fw.write("Stauerzeugungsfrequnz=" + Settings.Frequency.toString + System.getProperty("line.separator"))
      fw.write("Ausgabefrequenz=" + Settings.PathOutput.toString + System.getProperty("line.separator"))
      fw.write("Grundfaktor=" + Settings.Factor.toString + System.getProperty("line.separator"))
      fw.write("Maximalfaktor=" + Settings.MaxChange.toString + System.getProperty("line.separator"))
      fw.close();
      cancellables += context.system.scheduler.schedule(Duration.Zero, Duration(Settings.PathOutput, TimeUnit.MILLISECONDS), self, PathOutput())
    }
  }

  protected def receive = {
    //Beendet die Stauerzeugung
    case Cancel(cancel) =>
      cancel.cancel
    //Richtet die Stauerzeugung ein
    case SetUp() =>
      setUp
    //Stau erzeugen  
    case StartGen() =>
      startgen()
    //Pfad ausgeben  
    case PathOutput() =>
      pathOutput
    case ReadJam() =>
      readJam()
    // Initialisierung
    case Initialize =>
      init()
    // Lift-Session
    case liftSession: LiftSession =>
      this.liftSession = Some(liftSession)
    case m: Any =>
      log.warning("Unknown message: %s" format m.toString)
  }

  /**
   * Event-Handler, der nach dem Stoppen des Services augeführt wird.
   */
  override def postStop() {
    // Alle schedule-Aktionen stoppen
    for (cancellable <- cancellables)
      cancellable.cancel()
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

  case class Cancel(cancel: Cancellable)

  /**
   * Richtet die Stauerzeugung ein
   */
  case class SetUp()

  /**
   * Erzeugen eines Staus
   */
  case class StartGen()

  /**
   * Ausgeben des aktuellen Pfades
   */
  case class PathOutput()

  /**
   * Ließt eine Folge von Staus aus einer Datei
   */
  case class ReadJam()

  /**
   * Weg.
   *
   * @param way Weg.
   */
  case class Way(way: Box[AntWay], newSpeed: Box[Double])

  /**
   * Initialisierung
   */
  case object Initialize
}