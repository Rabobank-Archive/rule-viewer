package controllers.conversion

import controllers.conversion.ImplicitConversions.contextReads._
import org.scalarules.engine._
import play.api.data.validation.ValidationError
import play.api.libs.json._

object InputConverter {
  def convertToIndividualContext(inputJsValue: JsValue, factMap: Map[String, Fact[Any]]): (List[JsSuccess[Context]], List[JsError]) = {
    val jsResults: List[JsResult[Context]] = inputJsValue match {
      case c: JsObject => reads(c, factMap)
      case o: Any => List(JsError(ValidationError("No JsValues other than JsObject are allowed!", inputJsValue)))
    }

    val successes : List[JsSuccess[Context]] = jsResults.collect{ case succes: JsSuccess[Context] => succes }
    val errors: List[JsError] = jsResults.collect{ case error: JsError => error }

    (successes, errors)
  }
}
