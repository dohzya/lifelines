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
import lifelines.models.GameFile
import lifelines.models.GameFileParser
import lifelines.models.instructions
import lifelines.models.instructions.Instruction
import lifelines.models.messages

/** GameActor stores the infos of one game (one player) */
class GameActor(out: ActorRef, fast: Boolean) extends Actor {
  import GameActor.logger

  val game = GameActor.gameFile

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
      self ! instructions.Jump(game.metadata.start)
    case msg => Logger.warn(s"Received unsupported message: $msg")
  }

  object current {
    val ctx = scala.collection.mutable.Map[String, Int](game.ctx.data.toSeq: _*)
    var stack = Seq.empty[Instruction]
    var name = game.metadata.name
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

    case messages.Input(action) =>
      // TODO check that this action is allowed (store it when sending a choice)
      // send "choice1" and "choice2" to the user, and use-it to retrieve the step
      current.ctx += (action -> 1)
      self ! instructions.Jump(action)

    case instructions.Wait(duration, instr) =>
      self ! DoWait(duration, instr)

    case DoWait(duration, instr) =>
      schedule(duration, instr)

    case instructions.Talk(content) =>
      out ! messages.Talking
      val waitTime = (content.size * 50).milliseconds
      schedule(200.milliseconds, DoWait(waitTime, SendTalk(content)))

    case SendTalk(content) =>
      out ! messages.Talk(parseText(content))
      schedule(200.milliseconds, Next)

    case instructions.Info(content) =>
      out ! messages.Info(parseText(content))
      self ! Next

    case instructions.Question(choices) =>
      schedule(500.milliseconds, SendQuestion(choices))

    case SendQuestion(choices) =>
      out ! messages.Choices(choices.map {
        case (c, t) => c -> parseText(t)
      }.toMap)
      self ! Next

    case instructions.SetCtx(param, value) =>
      current.ctx += (param -> value)
      self ! Next

    case instructions.IncrCtx(param, value) =>
      val newValue = current.ctx.getOrElse(param, 0) + value
      current.ctx += (param -> newValue)
      self ! Next

    case instructions.DecrCtx(param, value) =>
      val newValue = current.ctx.getOrElse(param, 0) - value
      current.ctx += (param -> newValue)
      self ! Next

    case instructions.IfCtxEQ(param, value, instr) =>
      if (current.ctx.get(param).map(_ == value).getOrElse(false)) {
        self ! instr
      }
      else self ! Next

    case instructions.IfCtxGT(param, value, instr) =>
      if (current.ctx.get(param).map(_ > value).getOrElse(false)) {
        self ! instr
      }
      else self ! Next

    case instructions.IfCtxGTE(param, value, instr) =>
      if (current.ctx.get(param).map(_ >= value).getOrElse(false)) {
        self ! instr
      }
      else self ! Next

    case instructions.IfCtxLT(param, value, instr) =>
      if (current.ctx.get(param).map(_ < value).getOrElse(false)) {
        self ! instr
      }
      else self ! Next

    case instructions.IfCtxLTE(param, value, instr) =>
      if (current.ctx.get(param).map(_ <= value).getOrElse(false)) {
        self ! instr
      }
      else self ! Next

    case instructions.Jump(step) =>
      game.steps.get(step) match {
        case Some(instrs) =>
          current.stack = instrs
        case None =>
          Logger.debug(s"Unknown step '$step'")
          out ! messages.Error("<p>Action inconnue</p>")
          self ! PoisonPill
      }
      self ! Next

    case msg => Logger.warn(s"Received unsupported message: $msg")
  }

}
object GameActor {
  def logger = Logger("app.actors.game")

  val gameFile = PlayCurrent.resourceAsStream("game") match {
    case Some(content) => GameFileParser.parse(new java.io.InputStreamReader(content))
    case None => sys.error("Missing 'game' file")
  }

  def props(out: ActorRef, fast: Boolean) = Props(classOf[GameActor], out, fast)
}
