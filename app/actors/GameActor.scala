package lifelines
package actors

import akka.actor.PoisonPill
import akka.actor.{ Actor, Props, ActorRef }
import play.api.libs.concurrent.Akka
import play.api.libs.json.Json
import play.api.Logger
import play.api.Play.{ current => PlayCurrent }
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

import lifelines.models
import lifelines.models.instructions._

/** GameActor stores the infos of one game (one player) */
class GameActor(out: ActorRef, fast: Boolean) extends Actor {
  import GameActor.logger

  val steps = GameActor.steps

  var maybeId = Option.empty[Int]

  override def preStart() {
    logger.info("Start game")
    WorldActor ! WorldActor.Started(self)
  }

  override def postStop() {
    logger.info(s"""Stoped game ${maybeId.getOrElse("-")}""")
    WorldActor ! WorldActor.Stopped(maybeId)
  }

  case object Next
  case class SendTalk(content: String)
  case class SendQuestion(choices: Map[String, String])
  case class DoWait(duration: FiniteDuration, action: Any)

  def receive = receivedCreated

  def receivedCreated: Receive = {
    case WorldActor.SetId(id) =>
      maybeId = Some(id)
      context become receivedStarted
      self ! Next
    case msg => Logger.warn(s"Received unsupported message: $msg")
  }

  object current {
    val ctx = scala.collection.mutable.Map.empty[String, Int]
    var stack = steps("start")
    var name = "Crogon"
  }

  def schedule(duration: FiniteDuration, message: Any) = {
    if (fast) self ! message
    else context.system.scheduler.scheduleOnce(duration, self, message)
  }

  val parser = {
    import org.pegdown.{ Extensions => ext }
    val opts = Seq(
      ext.QUOTES, // Pretty single and double quotes.
      ext.SMARTS, // Pretty ellipses, dashes and apostrophes.
      ext.SMARTYPANTS, // All of the smartypants prettyfications.
      ext.SUPPRESS_ALL_HTML // new org.pegdown.PegDownProcessor()
    ).reduce(_ | _)
    new org.pegdown.PegDownProcessor(opts)
  }
  def parseText(text: String): String = {
    parser.markdownToHtml(text)
  }

  def receivedStarted: Receive = {
    case Next =>
      current.stack match {
        case instr +: stack =>
          self ! instr
          current.stack = stack
        case Seq() => // nothing to do
      }

    case models.Input(action) =>
      current.ctx += (action -> 1)
      self ! Jump(action)

    case Wait(duration, instr) =>
      self ! DoWait(duration, instr)

    case DoWait(duration, instr) =>
      schedule(duration, instr)

    case Talk(content) =>
      out ! models.Talking
      schedule(200.milliseconds, DoWait(1000.milliseconds, SendTalk(content)))

    case SendTalk(content) =>
      out ! models.Talk(parseText(content))
      schedule(200.milliseconds, Next)

    case Info(content) =>
      out ! models.Info(parseText(content))
      self ! Next

    case Question(choices) =>
      schedule(500.milliseconds, SendQuestion(choices))

    case SendQuestion(choices) =>
      out ! models.Choices(choices.map {
        case (c, t) => c -> parseText(t)
      }.toMap)
      self ! Next

    case SetCtx(param, value) =>
      current.ctx += (param -> value)
      self ! Next

    case IncrCtx(param, value) =>
      val newValue = current.ctx.getOrElse(param, 0) + value
      current.ctx += (param -> newValue)
      self ! Next

    case DecrCtx(param, value) =>
      val newValue = current.ctx.getOrElse(param, 0) - value
      current.ctx += (param -> newValue)
      self ! Next

    case IfCtxEQ(param, value, instr) =>
      if (current.ctx.get(param).map(_ == value).getOrElse(false)) {
        self ! instr
      }
      else self ! Next

    case IfCtxGT(param, value, instr) =>
      if (current.ctx.get(param).map(_ > value).getOrElse(false)) {
        self ! instr
      }
      else self ! Next

    case IfCtxLT(param, value, instr) =>
      if (current.ctx.get(param).map(_ < value).getOrElse(false)) {
        self ! instr
      }
      else self ! Next

    case Jump(step) =>
      steps.get(step) match {
        case Some(instructions) =>
          current.stack = instructions
        case None =>
          out ! models.Error("<p>Action inconnue</p>")
          self ! PoisonPill
      }
      self ! Next

    case msg => Logger.warn(s"Received unsupported message: $msg")
  }

}
object GameActor {
  def logger = Logger("app.actors.game")

  val steps = PlayCurrent.resourceAsStream("steps") match {
    case Some(content) => StepsParser.parse(new java.io.InputStreamReader(content))
    case None => sys.error("Missing steps file")
  }

  def props(out: ActorRef, fast: Boolean) = Props(classOf[GameActor], out, fast)
}
