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
import scala.util.parsing.combinator.RegexParsers

import lifelines.models

/** GameActor stores the infos of one game (one player) */
class GameActor(out: ActorRef, fast: Boolean) extends Actor {
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
  case class SetCtx(param: String, value: Int) extends Instruction
  case class IncrCtx(param: String, value: Int) extends Instruction
  case class DecrCtx(param: String, value: Int) extends Instruction
  case class IfCtxEQ(param: String, value: Int, instr: Instruction) extends Instruction
  case class IfCtxGT(param: String, value: Int, instr: Instruction) extends Instruction
  case class IfCtxLT(param: String, value: Int, instr: Instruction) extends Instruction
  case class Jump(step: String) extends Instruction

  case class SendTalk(content: String) extends Instruction
  case class SendQuestion(choices: Map[String, String]) extends Instruction

  case object Next

  type Steps = Map[String, Seq[Instruction]]

  object StepsParser extends RegexParsers {

    override def skipWhitespace = false

    def eol: Parser[Unit] = """\n""".r ^^ { _ => () }
    def indent: Parser[Unit] = "  " ^^ { _ => () }
    def ieol: Parser[Unit] = eol ~ indent ^^ { _ => () }
    def value: Parser[Int] = """\d+""".r ^^ { _.toInt }
    def id: Parser[String] = """[a-z]*""".r
    def text: Parser[String] = ".*".r

    def talk: Parser[Talk] = "\"" ~> text ^^ { Talk(_) }
    def setCtx: Parser[SetCtx] = (id <~ " = ") ~ value ^^ { case p ~ v => SetCtx(p, v) }
    def incrCtx: Parser[IncrCtx] = (id <~ " += ") ~ value ^^ { case p ~ v => IncrCtx(p, v) }
    def decrCtx: Parser[DecrCtx] = (id <~ " -= ") ~ value ^^ { case p ~ v => DecrCtx(p, v) }
    def ifCtx: Parser[IfCtxEQ] = (id <~ " ? ") ~ instr ^^ { case c ~ i => IfCtxEQ(c, 1, i) }
    def ifCtxEQ: Parser[IfCtxEQ] = (id <~ " = ") ~ (value <~ " ? ") ~ instr ^^ { case c ~ v ~ i => IfCtxEQ(c, v, i) }
    def ifCtxGT: Parser[IfCtxGT] = (id <~ " > ") ~ (value <~ " ? ") ~ instr ^^ { case c ~ v ~ i => IfCtxGT(c, v, i) }
    def ifCtxLT: Parser[IfCtxLT] = (id <~ " < ") ~ (value <~ " ? ") ~ instr ^^ { case c ~ v ~ i => IfCtxLT(c, v, i) }
    def jump: Parser[Jump] = "-> " ~> id ^^ { Jump(_) }
    def info: Parser[Info] = "-- " ~> text ^^ { Info(_) }
      def choice: Parser[(String, String)] = (id <~ " <- ") ~ text ^^ { case i ~ q => (i -> q) }
    def question: Parser[Question] = "(" ~> rep(ieol ~> indent ~> choice) <~ ieol <~ ")" ^^ { cs => Question(cs.toMap) }

    def instr: Parser[Instruction] = talk|setCtx|ifCtx|ifCtxEQ|ifCtxGT|ifCtxLT|jump|info|question
    def step: Parser[(String, Seq[Instruction])] = id ~ ":" ~ rep(ieol ~> instr) ^^ { case n ~ _ ~ i => n -> i }
    def steps: Parser[Steps] = opt(eol) ~> repsep(step, eol) <~ opt(eol) ^^ { _.toMap }

    def parse(input: String): Steps = parseAll(steps, input) match {
      case Success(result, _) => result
      case failure : NoSuccess => scala.sys.error(failure.msg)
    }
  }

  var steps = StepsParser.parse("""
start:
  " Euh… bonjour, vous me recevez ?
  (
    jevousrecois <- Je vous reçois.
    quiestce <- Qui est-ce ?
  )
quiestce:
  " On m'appelle Coca
  confiance = 30
  jevousrecois ? -> suite
  -> jevousrecois
jevousrecois:
  " Je viens de trouver un téléphone, mais je peux juste envoyer des messages
  " Apparemment vous me recevez.
  " C'est bien.
  confiance > 20 ? -> suite
  " Vous êtes dans un abris ?
  (
    suite <- Un abris ?
    maisquietesvous <- Mais qui êtes vous ?
  )
maisquietesvous:
  " Oh, désolé, j'aurais du commencer par là.
  -> quiestce
suite:
  -- to continue
""")

  object current {
    val ctx = scala.collection.mutable.Map.empty[String, Int]
    var stack = steps("start")
    var name = "Crogon"
  }

  def schedule(duration: FiniteDuration, message: Any) = {
    if (fast) self ! message
    else context.system.scheduler.scheduleOnce(duration, self, message)
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
      schedule(duration, instr)

    case Talk(content) =>
      out ! models.Talking
      schedule(200.milliseconds, Wait(1000.milliseconds, SendTalk(content)))

    case SendTalk(content) =>
      out ! models.Talk(content)
      schedule(200.milliseconds, Next)

    case Info(content) =>
      out ! models.Info(content)
      self ! Next

    case Question(choices) =>
      schedule(500.milliseconds, SendQuestion(choices))

    case SendQuestion(choices) =>
      out ! models.Choices(choices.toMap)
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
  def props(out: ActorRef, fast: Boolean) = Props(classOf[GameActor], out, fast)
}
