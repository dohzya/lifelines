package lifelines
package controllers

import play.api.libs.json.JsValue
import play.api.Logger
import play.api.mvc.WebSocket.FrameFormatter
import play.api.mvc.{ Action, Controller, WebSocket }
import play.api.Play.current

import lifelines.models.messages.{ Input, Output }

class Application extends Controller {

  def index = Action { req =>
    // val wsUri = routes.Application.create.webSocketURL(req)
    val protocol = if (req.secure) "wss" else "ws"
    val wsUri = s"""${protocol}://${req.host}/create"""
    Ok(views.html.index(wsUri))
  }

  implicit val inEventFrameFormatter = FrameFormatter.jsonFrame[Input]
  implicit val outEventFrameFormatter = FrameFormatter.jsonFrame[Output]

  def create = WebSocket.acceptWithActor[Input, Output] { request => out =>
    val fast = request.getQueryString("fast") == Some("true")
    lifelines.actors.GameActor.props(out, fast)
  }

}
