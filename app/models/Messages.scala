package lifelines
package models

import play.api.libs.json.{ Format, Json, JsError, JsResult, JsValue, JsObject, JsString }

case class Input(action: String)
object Input {
  implicit val jsonFormat = new Format[Input] {
    def reads(json: JsValue): JsResult[Input] = {
      (json \ "action").validate[String].map(Input(_))
    }
    def writes(input: Input): JsValue = {
      Json.obj("action" -> input.action)
    }
  }
}

sealed trait Output
case class Talk(content: String) extends Output
case class Info(content: String) extends Output
case class Error(content: String) extends Output
case class Choices(choices: Map[String, String]) extends Output
object Output {
  implicit val jsonFormat = new Format[Output] {
    def reads(json: JsValue): JsResult[Output] = {
      (json \ "kind").validate[String].flatMap {
        case "talk" => (json \ "message").validate[String].map(Talk(_))
        case "info" => (json \ "message").validate[String].map(Info(_))
        case "error" => (json \ "message").validate[String].map(Error(_))
        case "choices" => (json \ "choices").validate[Map[String, String]].map(Choices(_))
        case _ => JsError("Unknown kind")
      }
    }
    def writes(o: Output): JsValue = o match {
      case Talk(content) => Json.obj("kind" -> "talk", "message" -> content)
      case Info(content) => Json.obj("kind" -> "info", "message" -> content)
      case Error(content) => Json.obj("kind" -> "error", "message" -> content)
      case Choices(choices) => Json.obj(
        "kind" -> "choices",
        "choices" -> JsObject(choices.toSeq.map{ case (n, v) => n -> JsString(v) })
      )
    }
  }
}
