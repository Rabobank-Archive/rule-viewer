package controllers.conversion

import controllers.conversion.ImplicitConversions.contextReads._
import controllers.conversion.ImplicitConversions.contextWrites._
import org.scalarules.engine._
import play.api.data.validation.ValidationError
import play.api.libs.json._

object Converter {
  def convertToIndividualContext(inputJsValue: JsValue, factMap: Map[String, Fact[Any]], jsonConversionMap: JsonConversionsProvider): (List[JsSuccess[Context]], List[JsError]) = {
    val jsResults: List[JsResult[Context]] = inputJsValue match {
      case c: JsObject => reads(c, factMap, jsonConversionMap)
      case o: Any => List(JsError(ValidationError("No JsValues other than JsObject are allowed!", inputJsValue)))
    }

    val successes : List[JsSuccess[Context]] = jsResults.collect{ case success: JsSuccess[Context] => success }
    val errors: List[JsError] = jsResults.collect{ case error: JsError => error }

    (successes, errors)
  }

  def contextToJson(context: Context, jsonConversionMap: JsonConversionsProvider): JsValue = writes(context, jsonConversionMap)
}
