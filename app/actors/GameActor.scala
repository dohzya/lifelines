package lifelines
package actors

import akka.actor.PoisonPill
import akka.actor.{ Actor, Props, ActorRef }
import play.api.libs.concurrent.Akka
import play.api.libs.json.Json
import play.api.Logger
import play.api.Play.current
import scala.concurrent.duration._

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
  case class Talk(content: String) extends Instruction
  case class Info(content: String) extends Instruction
  case class Question(choices: Map[String, String]) extends Instruction
    object Question { def apply(choices: (String, String)*): Question = Question(choices.toMap) }
  case class SetCtx(param: (String, Int)) extends Instruction
  case class IfCtxEQ(param: (String, Int), instr: Instruction) extends Instruction
  case class IfCtxGT(param: (String, Int), instr: Instruction) extends Instruction
  case class IfCtxLT(param: (String, Int), instr: Instruction) extends Instruction
  case class Jump(step: String) extends Instruction

  case object Next

  var steps = Map[String, Seq[Instruction]](
    "start" -> Seq(
      Talk("Salut"),
      Talk("Comment vas-tu ?"),
      Question(
        "fine" -> "Ça va bien, merci.",
        "badly" -> "Euh… là tout de suite, moyen."
      )
    ),
    "fine" -> Seq(
      SetCtx("happy" -> 1),
      Talk("Cool"),
      Jump("start2")
    ),
    "badly" -> Seq(
      SetCtx("happy" -> 0),
      Talk("Ha, désolé"),
      Jump("start2")
    ),
    "start2" -> Seq(
      Talk("J'ai une mission pour toi"),
      IfCtxEQ("happy" -> 0, Talk("malgré ton mal à la vie")),
      IfCtxEQ("happy" -> 0, Talk("mais remarque je m'en fou")),
      Talk("Es-tu partant ?"),
      Question(
        "partant" -> "Bien sur.",
        "papartant" -> "Euh… pas vraiment, non."
      )
    ),
    "partant" -> Seq(Talk("cool")),
    "papartant" -> Seq(Info("The end"))
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
      logger.debug(s"Received action $action")
      self ! Jump(action)

    case Talk(content) =>
      out ! models.Talk(content)
      self ! Next

    case Info(content) =>
      out ! models.Info(content)
      self ! Next

    case Question(choices) =>
      out ! models.Choices(choices.toMap)
      self ! Next

    case SetCtx((param, value)) =>
      current.ctx += (param -> value)
      self ! Next

    case IfCtxEQ((param, value), instr) =>
      if (current.ctx.get(param).map(_ == value).getOrElse(false)) {
        self ! instr
      }
      self ! Next

    case IfCtxGT((param, value), instr) =>
      if (current.ctx.get(param).map(_ > value).getOrElse(false)) {
        self ! instr
      }
      self ! Next

    case IfCtxLT((param, value), instr) =>
      if (current.ctx.get(param).map(_ < value).getOrElse(false)) {
        self ! instr
      }
      self ! Next

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
