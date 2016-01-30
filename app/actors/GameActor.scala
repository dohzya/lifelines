package lifelines
package actors

import akka.actor.PoisonPill
import akka.actor.{ Actor, Props, ActorRef }
import play.api.libs.concurrent.Akka
import play.api.libs.json.Json
import play.api.Logger
import play.api.Play.current
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

import lifelines.models

/** GameActor stores the infos of one game (one player) */
class GameActor(out: ActorRef) extends Actor {
  import GameActor.logger

  var maybeId = Option.empty[Int]

  override def preStart() {
    logger.info("Start game")
    WorldActor ! WorldActor.Started(self)
  }

  override def postStop() {
    logger.info(s"""Stoped game ${maybeId.getOrElse("-")}""")
    WorldActor ! WorldActor.Stopped(maybeId)
  }

  def receive = receivedCreated

  def receivedCreated = buildReceive {
    case WorldActor.SetId(id) =>
      maybeId = Some(id)
      context become receivedStarted
      self ! Next
  }

  sealed trait Instruction
  case class Wait(duration: FiniteDuration, instr: Instruction) extends Instruction
  case class Talk(content: String) extends Instruction
  case class Info(content: String) extends Instruction
  case class Question(choices: Map[String, String]) extends Instruction
    object Question { def apply(choices: (String, String)*): Question = Question(choices.toMap) }
  case class SetCtx(param: (String, Int)) extends Instruction
  case class IfCtxEQ(param: (String, Int), instr: Instruction) extends Instruction
  case class IfCtxGT(param: (String, Int), instr: Instruction) extends Instruction
  case class IfCtxLT(param: (String, Int), instr: Instruction) extends Instruction
  case class Jump(step: String) extends Instruction

  case class SendTalk(content: String) extends Instruction
  case class SendQuestion(choices: Map[String, String]) extends Instruction

  case object Next

  var steps = Map[String, Seq[Instruction]](
    "start" -> Seq(
      Talk("Euh… bonjour, vous me recevez ?"),
      Question(
        "jevousrecois" -> "Je vous reçois.",
        "quiestce" -> "Qui est-ce ?"
      )
    ),
    "quiestce" -> Seq(
      Talk("On m'appelle Coca"),
      IfCtxEQ("jevousrecois" -> 1, Jump("suite")),
      Jump("jevousrecois")
    ),
    "jevousrecois" -> Seq(
      Talk("Je viens de trouver un téléphone, mais je peux juste envoyer des messages"),
      Talk("Apparemment vous me recevez."),
      Talk("C'est bien."),
      IfCtxEQ("quiestce" -> 1, Jump("suite")),
      Talk("Vous êtes dans un abris ?"),
      Question(
        "suite" -> "Un abris ?",
        "maisquietesvous" -> "Mais qui êtes vous ?"
      )
    ),
    "maisquietesvous" -> Seq(
      Talk("Oh, désolé, j'aurais du commencer par là."),
      Jump("quiestce")
    ),
    "suite" -> Seq(
      Info("-- to continue --")
    )
  )

  object current {
    val ctx = scala.collection.mutable.Map.empty[String, Int]
    var stack = steps("start")
    var name = "Crogon"
  }

  def receivedStarted = buildReceive {
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
      context.system.scheduler.scheduleOnce(duration, self, instr)

    case Talk(content) =>
      out ! models.Talking
      context.system.scheduler.scheduleOnce(200.milliseconds, self, Wait(1000.milliseconds, SendTalk(content)))

    case SendTalk(content) =>
      out ! models.Talk(content)
      self ! Next

    case Info(content) =>
      out ! models.Info(content)
      self ! Next

    case Question(choices) =>
      context.system.scheduler.scheduleOnce(500.milliseconds, self, SendQuestion(choices))

    case SendQuestion(choices) =>
      out ! models.Choices(choices.toMap)
      self ! Next

    case SetCtx((param, value)) =>
      current.ctx += (param -> value)
      self ! Next

    case IfCtxEQ((param, value), instr) =>
      if (current.ctx.get(param).map(_ == value).getOrElse(false)) {
        self ! instr
      }
      else self ! Next

    case IfCtxGT((param, value), instr) =>
      if (current.ctx.get(param).map(_ > value).getOrElse(false)) {
        self ! instr
      }
      else self ! Next

    case IfCtxLT((param, value), instr) =>
      if (current.ctx.get(param).map(_ < value).getOrElse(false)) {
        self ! instr
      }
      else self ! Next

    case Jump(step) =>
      steps.get(step) match {
        case Some(instructions) =>
          current.stack = instructions
        case None =>
          out ! models.Error("Action inconnue")
          self ! PoisonPill
      }
      self ! Next
  }

  // helpers

  private def buildReceive(pf: Receive): Receive = {
    pf.orElse { case msg => Logger.warn(s"Received unsupported message: $msg") }
  }

}
object GameActor {
  def logger = Logger("app.actors.game")
  def props(out: ActorRef) = Props(classOf[GameActor], out)
}
