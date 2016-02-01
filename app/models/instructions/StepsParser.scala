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
    def id: Parser[String] = """[a-z][a-z_0-9]*""".r
    def text: Parser[String] = ".*".r

    def comment: Parser[String] = "# *".r ~> ".*".r <~ eol

        def talkSingle: Parser[String] = "\" " ~> text <~ eol
        def talkMulti: Parser[String] = "\"" ~> eol ~> rep1(indent ~> indent ~> text <~ eol | eol) ^^ { _.mkString("\n") }
    def talk: Parser[Talk] = (talkSingle | talkMulti) ^^ { Talk(_) }
    def setCtx: Parser[SetCtx] = (id <~ " = ") ~ value <~ eol ^^ { case p ~ v => SetCtx(p, v) }
    def incrCtx: Parser[IncrCtx] = (id <~ " += ") ~ value <~ eol ^^ { case p ~ v => IncrCtx(p, v) }
    def decrCtx: Parser[DecrCtx] = (id <~ " -= ") ~ value <~ eol ^^ { case p ~ v => DecrCtx(p, v) }
    def ifCtx: Parser[IfCtxEQ] = (id <~ " ? ") ~ instr ^^ { case c ~ i => IfCtxEQ(c, 1, i) }
    def ifCtxEQ: Parser[IfCtxEQ] = (id <~ " = ") ~ (value <~ " ? ") ~ instr ^^ { case c ~ v ~ i => IfCtxEQ(c, v, i) }
    def ifCtxGT: Parser[IfCtxGT] = (id <~ " > ") ~ (value <~ " ? ") ~ instr ^^ { case c ~ v ~ i => IfCtxGT(c, v, i) }
    def ifCtxGTE: Parser[IfCtxGTE] = (id <~ " >= ") ~ (value <~ " ? ") ~ instr ^^ { case c ~ v ~ i => IfCtxGTE(c, v, i) }
    def ifCtxLT: Parser[IfCtxLT] = (id <~ " < ") ~ (value <~ " ? ") ~ instr ^^ { case c ~ v ~ i => IfCtxLT(c, v, i) }
    def ifCtxLTE: Parser[IfCtxLTE] = (id <~ " <= ") ~ (value <~ " ? ") ~ instr ^^ { case c ~ v ~ i => IfCtxLTE(c, v, i) }
    def jump: Parser[Jump] = "-> " ~> id <~ eol ^^ { Jump(_) }
    def info: Parser[Info] = "-- " ~> text <~ eol ^^ { Info(_) }
      def choice: Parser[(String, String)] = (id <~ " <- ") ~ text <~ eol ^^ { case i ~ q => (i -> q) }
    def question: Parser[Question] = "(" ~> eol ~> rep1(indent ~> indent ~> (choice | comment)) <~ indent <~ ")" <~ eol ^^ {
        cs => Question(cs.collect { case c: (String, String) @unchecked => c }.toMap)
    }

    def instr: Parser[Instruction] = talk|setCtx|incrCtx|decrCtx|ifCtx|ifCtxEQ|ifCtxGT|ifCtxGTE|ifCtxLT|ifCtxLTE|jump|info|question
    def step: Parser[(String, Seq[Instruction])] = (id <~ ":" <~ opt(" #.*".r) <~ eol) ~ rep(indent ~> (instr | comment | eol)) ^^ {
      case n ~ i => n -> i.collect { case i: Instruction => i }
    }
    def steps: Parser[Steps] = opt(eol) ~> rep(step | comment | eol) <~ opt(eol) ^^ {
        _.collect { case s: (String, Seq[Instruction]) @unchecked => s }.toMap
    }

    def parse(input: java.io.InputStreamReader): Steps = parseAll(steps, input) match {
      case Success(result, _) => result
      case failure : NoSuccess => scala.sys.error(failure.toString)
    }
  }
