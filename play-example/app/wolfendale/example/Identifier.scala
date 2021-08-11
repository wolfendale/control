package wolfendale.example

import play.api.mvc.Call

sealed abstract class Identifier(val sessionKey: String, val call: Call) extends Product with Serializable

object Identifier {

  case object A extends Identifier("A", controllers.routes.IndexController.getA())
  case object B extends Identifier("B", controllers.routes.IndexController.getB())
  case object C extends Identifier("C", controllers.routes.IndexController.getC())
  case object CheckYourAnswers extends Identifier("Check Your Answers", controllers.routes.IndexController.getCheckYourAnswers())
}