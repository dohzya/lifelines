package lifelines
package models

import lifelines.models.instructions.Instruction

case class GameFile(
  metadata: GameFile.Metadata,
  ctx: GameFile.Context,
  css: String,
  steps: GameFile.Steps
)
object GameFile {

  case class Metadata(
    name: String,
    world: String,
    typingspeed: Metadata.TypingSpeed,
    start: String
  )
  object Metadata {

    sealed trait TypingSpeed {
      def name: String
      def factor: Long
    }
    object TypingSpeed {
      case object Slow extends TypingSpeed {
        def name = "slow"
        def factor = 2l
      }
      case object Fast extends TypingSpeed {
        def name = "fast"
        def factor = 1l
      }
      case object Instant extends TypingSpeed {
        def name = "instant"
        def factor = 0l
      }
      def all: Seq[TypingSpeed] = Seq(Slow, Fast, Instant)
      def get(name: String) = all.find(_.name == name)
      def apply(name: String) = get(name).getOrElse(sys.error(s"Unknown typingspeed '$name'"))
    }

  }

  case class Context(data: Map[String, Int])

  case class Steps(data: Map[String, Seq[Instruction]]) {
    def get(param: String) = data.get(param)
  }

}
