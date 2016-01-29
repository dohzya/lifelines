package lifelines
package actors

import akka.actor.PoisonPill
import akka.actor.{ Actor, Props, ActorRef }
import play.api.libs.concurrent.Akka
import play.api.libs.json.Json
import play.api.Logger
import play.api.Play.current

import lifelines.models.{ Choices, Error, Info, Input, Output, Talk }

/** GameActor stores the infos of one game (one player) */
class GameActor(out: ActorRef) extends Actor {

  var maybeId = Option.empty[Int]

  override def preStart() {
    Logger.info("Start game")
    WorldActor ! WorldActor.Started(self)
  }

  def receive = receivedCreated

  def receivedCreated = buildReceive {
    case WorldActor.SetId(id) =>
      maybeId = Some(id)
      context become receivedStarted
      out ! Talk("hi")
  }

  def receivedStarted = buildReceive {
    case input: Input =>
      Logger.info(s"Received $input")
      Thread.sleep(1000)
      out ! Talk("How are you?")
      Thread.sleep(1000)
      out ! Choices(Map(
        "fine" -> "I'm find, thanks",
        "badly" -> "I'm a bit tired right now"
      ))
  }

  // helpers

  private def buildReceive(pf: Receive): Receive = {
    pf.orElse { case msg => Logger.warn(s"Received unsupported message: $msg") }
  }

}
object GameActor {
  def props(out: ActorRef) = Props(classOf[GameActor], out)
}
