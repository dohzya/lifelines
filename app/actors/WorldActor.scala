package lifelines
package actors

import akka.actor.{ Actor, Props, ActorRef }
import play.api.libs.concurrent.Akka
import play.api.Logger
import play.api.Play.current
import scala.concurrent.Promise

/** WorldActor stores the infos of the whole world */
class WorldActor extends Actor {

  val logger = Logger("app.actors.world")

  override def preStart() {
    logger.info("Start world")
  }

  override def postStop() {
    logger.info("Stopped world")
  }

  var lastId = 0
  val games = scala.collection.mutable.Map.empty[Int, ActorRef]

  def createGame(ref: ActorRef): Int = {
    lastId += 1
    games += (lastId -> ref)
    lastId
  }

  def receive = {
    case WorldActor.Started(ref) =>
      val id = createGame(ref)
      ref ! WorldActor.SetId(id)
      logger.debug(s"""${games.size} games - lastId = $lastId""")
    case WorldActor.Stopped(Some(id)) =>
      games -= id
      logger.debug(s"""${games.size} games - lastId = $lastId""")
    case WorldActor.Stopped(None) => // nothing to do
    case msg =>
      logger.warn(s"Received unsupported message: $msg")
  }

}

object WorldActor {

  sealed trait Messages
  case class Stopped(maybeId: Option[Int]) extends Messages
  case class Started(ref: ActorRef) extends Messages
  case class SetId(id: Int) extends Messages

  private val ref = Akka.system.actorOf(Props[WorldActor], name = "world")
  def !(msg: Messages) = ref ! msg

}
