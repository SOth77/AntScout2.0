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

  /**
   * Stauerzeugung
   */
  def startgen() {
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
    val speed = actElem.maxSpeed(true) * changeNumber * 3.6
    if (speed >= 1.0) {
      log.info("Stauerzeugung - Zufall: Geschwindigkeit von %s Kilometer pro Stunde für Knoten %s".format(speed, actElem.id))
      NamedCometListener.getDispatchersFor(Full("userInterface")) foreach { actor =>
        actor.map(_ ! Way(Full(actElem), Full(speed)))
      }
      if (Settings.SaveJam != "empty") {
        val fw = new FileWriter(Settings.SaveJam, true)
        fw.write(actElem.id + "," + speed.toString + System.getProperty("line.separator"))
        fw.close();
      }
    }
  }

  /**
   * Pfad ausgeben
   */
  def pathOutput {
    if (Settings.JamActive) {
      for {
        liftSession <- liftSession
      } yield {
        S.initIfUninitted(liftSession) {
          for {
            path <- Path
          } yield {
            val (length, tripTime) = path.foldLeft(0.0, 0.0) {
              case ((lengthAcc, tripTimeAcc), way) => (way.length + lengthAcc, way.tripTime + tripTimeAcc)
            }
            var string = "empty"
            if (Settings.Dji && Settings.SaveDijkstra != "empty") {
              string = Settings.SaveDijkstra
            } else if (Settings.Dji == false && Settings.SaveAnt != "empty") {
              string = Settings.SaveAnt
            }
            val fw = new FileWriter(string, true)
            fw.write(length.toString + "," + tripTime.toString + System.getProperty("line.separator"))
            fw.close();
          }
        }
      }
    }
  }

  def readJam() {
    try {
      val iter = Settings.Lines
      if (iter.hasNext) {
        val args = iter.next.split(",")
        val actElem = AntMap.ways.find(_.id == args(0)).get
        val speed = args(1).toDouble
        log.info("Stauerzeugung - Laden: Geschwindigkeit  %s Kilometer pro Stunde für Knoten %s".format(speed, actElem.id))
        NamedCometListener.getDispatchersFor(Full("userInterface")) foreach { actor =>
          actor.map(_ ! Way(Full(actElem), Full(speed)))
        }
      } else {
        Settings.JamActive = false;
      }
    } catch {
      case ex: Exception => //TODO: Fehlerbehandlung
    }
  }

  // Erzeugen der Schedules für Stau und Ausgabe
  def setUp {
    Settings.JamActive = true;
    if (Settings.LoadJam == "empty") {
      //Schedule für zufallsbasierte Stauerzeugung starten  
      cancellables += context.system.scheduler.schedule(Duration.Zero, Duration(Settings.Frequency,
        TimeUnit.MILLISECONDS), self, StartGen())
    } else {
      //Schedule für vordefinierte Stauerzeugung starten 
      val file = Source.fromFile(Settings.LoadJam)
      Settings.Lines = file.getLines()
      cancellables += context.system.scheduler.schedule(Duration.Zero, Duration(Settings.Frequency,
        TimeUnit.MILLISECONDS), self, ReadJam())
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
      fw.write("Positive=" + Settings.Positive.toString + System.getProperty("line.separator"))
      fw.write("Frequency=" + Settings.Frequency.toString + System.getProperty("line.separator"))
      fw.write("PathOutput=" + Settings.PathOutput.toString + System.getProperty("line.separator"))
      fw.write("Factory=" + Settings.Factor.toString + System.getProperty("line.separator"))
      fw.write("MaxChange=" + Settings.MaxChange.toString + System.getProperty("line.separator"))
      fw.write("C1=" + Settings.C1.toString + System.getProperty("line.separator"))
      fw.write("C2=" + Settings.C2.toString + System.getProperty("line.separator"))
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