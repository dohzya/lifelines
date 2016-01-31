package lifelines
package models
package instructions

import scala.util.parsing.combinator.RegexParsers
import scala.concurrent.duration._

object StepsParser extends RegexParsers {

    override def skipWhitespace = false

    def eol: Parser[Unit] = """\n""".r ^^ { _ => () }
    def indent: Parser[Unit] = "  " ^^ { _ => () }
    def ieol: Parser[Unit] = eol ~ indent ^^ { _ => () }
    def value: Parser[Int] = """\d+""".r ^^ { _.toInt }
    def id: Parser[String] = """[a-z]*""".r
    def text: Parser[String] = ".*".r

    def talk: Parser[Talk] = "\" " ~> text ^^ { Talk(_) }
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

    def parse(input: java.io.InputStreamReader): Steps = parseAll(steps, input) match {
      case Success(result, _) => result
      case failure : NoSuccess => scala.sys.error(failure.msg)
    }
  }
