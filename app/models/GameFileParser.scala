package lifelines
package models

import scala.util.parsing.combinator.RegexParsers
import scala.concurrent.duration._

import lifelines.models.instructions._

object GameFileParser extends RegexParsers {

    override def skipWhitespace = false

    def debug[A](parser: Parser[A])(name: String) = parser
    // def debug[A](parser: Parser[A])(name: String) = log(parser)(name)

    def eol: Parser[String] = debug("""\n""".r ^^ { _ => "\n" })("eol")
    def indent: Parser[String] = debug("  ")("indent")
    def spc1: Parser[String] = debug(" ")("spc1")
    def spc: Parser[String] = debug(""" +""".r)("spc")
    def value: Parser[Int] = debug("""\d+""".r ^^ { _.toInt })("value")
    def id: Parser[String] = debug("""[a-z][a-z_0-9]*""".r)("id")
    def text: Parser[String] = debug("""[^\n]+""".r)("text")

    def comment: Parser[String] = debug("#" ~> spc ~> ".*".r <~ eol)("comment")

    def talk: Parser[Talk] = debug {
      def talkSingle: Parser[String] = "\" " ~> text <~ eol
      def talkMulti: Parser[String] =
        "\"" ~> eol ~> rep1(indent ~> indent ~> text <~ eol | eol) ^^ { _.mkString("\n") }
      (talkSingle | talkMulti) ^^ { Talk(_) }
    }("talk")
    def setCtx: Parser[SetCtx] = debug {
      (id <~ spc <~ "=" <~ spc) ~ value <~ opt(spc ~> comment) <~ eol ^^ {
        case p ~ v => SetCtx(p, v)
      }
    }("setCtx")
    def incrCtx: Parser[IncrCtx] = debug {
      (id <~ spc <~ "+=" <~ spc) ~ value <~ opt(spc ~> comment) <~ eol ^^ {
        case p ~ v => IncrCtx(p, v)
      }
    }("incrCtx")
    def decrCtx: Parser[DecrCtx] = debug {
      (id <~ spc <~ "-=" <~ spc) ~ value <~ opt(spc ~> comment) <~ eol ^^ {
        case p ~ v => DecrCtx(p, v)
      }
    }("decrCtx")
    def ifCtx: Parser[IfCtxEQ] = debug {
      (id <~ spc <~ "?" <~ spc) ~ instr ^^ { case c ~ i => IfCtxEQ(c, 1, i) }
    }("ifCtx")
    def ifCtxEQ: Parser[IfCtxEQ] = debug {
      (id <~ spc <~ "=" <~ spc) ~ (value <~ spc <~ "?" <~ spc) ~ instr ^^ {
        case c ~ v ~ i => IfCtxEQ(c, v, i)
      }
    }("ifCtxEQ")
    def ifCtxGT: Parser[IfCtxGT] = debug {
      (id <~ spc <~ ">" <~ spc) ~ (value <~ spc <~ "?" <~ spc) ~ instr ^^ {
        case c ~ v ~ i => IfCtxGT(c, v, i)
      }
    }("ifCtxGT")
    def ifCtxGTE: Parser[IfCtxGTE] = debug {
      (id <~ spc <~ ">=" <~ spc) ~ (value <~ spc <~ "?" <~ spc) ~ instr ^^ {
        case c ~ v ~ i => IfCtxGTE(c, v, i)
      }
    }("ifCtxGTE")
    def ifCtxLT: Parser[IfCtxLT] = debug {
      (id <~ spc <~ "<" <~ spc) ~ (value <~ spc <~ "?" <~ spc) ~ instr ^^ {
        case c ~ v ~ i => IfCtxLT(c, v, i)
      }
    }("ifCtxLT")
    def ifCtxLTE: Parser[IfCtxLTE] = debug {
      (id <~ spc <~ "<=" <~ spc) ~ (value <~ spc <~ "?" <~ spc) ~ instr ^^ {
        case c ~ v ~ i => IfCtxLTE(c, v, i)
      }
    }("ifCtxLTE")
    def jump: Parser[Jump] = debug {
      "->" ~> spc ~> id <~ opt(spc ~> comment) <~ eol ^^ { Jump(_) }
    }("jump")
    def info: Parser[Info] = {
      "--" ~> spc ~> text <~ eol ^^ { Info(_) }
    }
    def question: Parser[Question] = debug {
      def choice: Parser[(String, String)] = {
        (id <~ " <- ") ~ text <~ eol ^^ { case i ~ q => (i -> q) }
      }
      "(" ~> eol ~> rep1(indent ~> indent ~> (choice | comment)) <~ indent <~ ")" <~ eol ^^ {
        cs => Question(cs.collect { case c: (String, String) @unchecked => c }.toMap)
      }
    }("question")

    def instr: Parser[Instruction] = debug {
      talk|setCtx|incrCtx|decrCtx|ifCtx|ifCtxEQ|ifCtxGT|ifCtxGTE|ifCtxLT|ifCtxLTE|jump|info|question
    }("instr")
    def step: Parser[(String, Seq[Instruction])] = debug {
      debug(id <~ ":" <~ (spc ~> comment | eol))("stepid") ~ rep(indent ~> (instr | comment)) ^^ {
        case n ~ i => n -> i.collect { case i: Instruction => i }
      }
    }("step")
    def steps: Parser[GameFile.Steps] = debug {
      rep(debug(step | comment | eol)("insteps")) ^^ {
        case s => GameFile.Steps(s.collect { case s: (String, Seq[Instruction]) @unchecked => s }.toMap)
      }
    }("steps")

    def headingSession: Parser[String] = debug("--- ")("headingSession")

    def sectionMetadata: Parser[GameFile.Metadata] = debug {
      def cs = ":" ~ spc ^^ { _ => () }
      def repc = rep(comment <~ eol)
      def name = "name" ~> cs ~> text <~ eol
      def world = "world" ~> cs ~> text <~ eol
      def typingspeed = {
        "typingspeed" ~> cs ~> ("slow"|"fast"|"instant") <~ eol ^^ {
          case s => GameFile.Metadata.TypingSpeed(s)
        }
      }
      def start = "start" ~> cs ~> text <~ eol
      headingSession ~> ("metadata" ~> eol ~> repc ~> name) ~ (repc ~> world) ~ (repc ~> typingspeed) ~ (repc ~> start <~ repc) ^^ {
        case n ~ w ~ t ~ s => GameFile.Metadata(n, w, t, s)
      }
    }("sectionMetadata")

    def sectionCtx: Parser[GameFile.Context] = debug {
      def ctx: Parser[(String, Int)] = {
        (id <~ spc <~ "=" ~ spc) ~ value <~ opt(spc ~> comment) ~ eol ^^ { case i ~ v => i -> v }
      }
      headingSession ~> "ctx" ~> eol ~> rep(ctx | comment <~ eol) ^^ {
        case cs => GameFile.Context(cs.collect { case c: (String, Int) @unchecked => c }.toMap)
      }
    }("sectionCtx")

    def sectionCss: Parser[String] = debug {
      headingSession ~> "css" ~> eol ~> rep(opt(not("---") ~> text) <~ eol ^^ { _.getOrElse("") }) ^^ { _.mkString("\n") }
    }("sectionCss")

    def sectionSteps: Parser[GameFile.Steps] = debug {
      headingSession ~> "steps" ~> eol ~> steps
    }("sectionSteps")

    def file: Parser[GameFile] = debug {
      sectionMetadata ~ sectionCtx ~ sectionCss ~ sectionSteps ^^ {
        case metadata ~ ctx ~ css ~ steps => GameFile(metadata, ctx, css, steps)
      }
    }("file")

    def parse(input: java.io.InputStreamReader): GameFile = parseAll(file, input) match {
      case Success(result, _) => result
      case failure : NoSuccess => scala.sys.error(failure.toString)
    }
  }
