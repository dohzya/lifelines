package lifelines
package models
package instructions

import scala.util.parsing.combinator.RegexParsers
import scala.concurrent.duration._

object StepsParser extends RegexParsers {

    override def skipWhitespace = false

    def eol: Parser[String] = """\n""".r ^^ { _ => "\n" }
    def indent: Parser[String] = "  "
    def value: Parser[Int] = """\d+""".r ^^ { _.toInt }
    def id: Parser[String] = """[a-z]*""".r
    def text: Parser[String] = ".*".r

    def comment: Parser[Unit] = " *#.*".r <~ eol ^^ { _ => () }

        def talkSingle: Parser[String] = "\" " ~> text <~ eol
        def talkMulti: Parser[String] = "\"" ~> eol ~> rep(indent ~> indent ~> text <~ eol | eol) ^^ { _.mkString("\n") }
    def talk: Parser[Talk] = (talkSingle | talkMulti) ^^ { Talk(_) }
    def setCtx: Parser[SetCtx] = (id <~ " = ") ~ value <~ eol ^^ { case p ~ v => SetCtx(p, v) }
    def incrCtx: Parser[IncrCtx] = (id <~ " += ") ~ value <~ eol ^^ { case p ~ v => IncrCtx(p, v) }
    def decrCtx: Parser[DecrCtx] = (id <~ " -= ") ~ value <~ eol ^^ { case p ~ v => DecrCtx(p, v) }
    def ifCtx: Parser[IfCtxEQ] = (id <~ " ? ") ~ instr ^^ { case c ~ i => IfCtxEQ(c, 1, i) }
    def ifCtxEQ: Parser[IfCtxEQ] = (id <~ " = ") ~ (value <~ " ? ") ~ instr ^^ { case c ~ v ~ i => IfCtxEQ(c, v, i) }
    def ifCtxGT: Parser[IfCtxGT] = (id <~ " > ") ~ (value <~ " ? ") ~ instr ^^ { case c ~ v ~ i => IfCtxGT(c, v, i) }
    def ifCtxLT: Parser[IfCtxLT] = (id <~ " < ") ~ (value <~ " ? ") ~ instr ^^ { case c ~ v ~ i => IfCtxLT(c, v, i) }
    def jump: Parser[Jump] = "-> " ~> id <~ eol ^^ { Jump(_) }
    def info: Parser[Info] = "-- " ~> text <~ eol ^^ { Info(_) }
      def choice: Parser[(String, String)] = (id <~ " <- ") ~ text <~ eol ^^ { case i ~ q => (i -> q) }
    def question: Parser[Question] = "(" ~> eol ~> rep(indent ~> indent ~> choice) <~ indent <~ ")" <~ eol ^^ { cs => Question(cs.toMap) }

    def instr: Parser[Instruction] = talk|setCtx|ifCtx|ifCtxEQ|ifCtxGT|ifCtxLT|jump|info|question
    def step: Parser[(String, Seq[Instruction])] = (id <~ ":" <~ eol) ~ rep(indent ~> (instr | comment)) ^^ {
      case n ~ i => n -> i.collect { case i: Instruction => i }
    }
    def steps: Parser[Steps] = opt(eol) ~> rep(step) <~ opt(eol) ^^ { _.toMap }

    def parse(input: java.io.InputStreamReader): Steps = parseAll(steps, input) match {
      case Success(result, _) => result
      case failure : NoSuccess => scala.sys.error(failure.toString)
    }
  }
