package controllers.conversion

import org.scalarules.engine._
import org.scalarules.finance.nl._
import play.api.data.validation.ValidationError
import play.api.libs.json._
import controllers.conversion.ImplicitConversions._

import scala.annotation.tailrec

object InputConverter {
  def convertToIndividualContext(inputJsValue: JsValue, factMap: Map[String, Fact[Any]]): (List[JsSuccess[Context]], List[JsError]) = {
    val jsResults: List[JsResult[Context]] = inputJsValue match {
      case c: JsObject => ImplicitConversions.factFormat.reads(c, factMap)
      case o: Any => List(JsError(ValidationError("No JsValues other than JsObject are allowed!", inputJsValue)))
    }

    val successes : List[JsSuccess[Context]] = jsResults.collect{ case succes: JsSuccess[Context] => succes }
    val errors: List[JsError] = jsResults.collect{ case error: JsError => error }

    (successes, errors)
  }
}

object OutputConverter {

  def buildJsonFromContext(context: Context) : JsValue = {
    @tailrec
    def inner(list: List[(Fact[Any], Any)], js: JsObject): JsValue = list match {
      case Nil => js
      case (fact, string: String) :: tail => inner(tail, js + (fact.name -> Json.toJson(string.toString)))
      case (fact, bedrag: Bedrag) :: tail => inner(tail, js + (fact.name -> Json.toJson[Bedrag](bedrag)))
      case (fact, percentage: Percentage) :: tail => inner(tail, js + (fact.name -> Json.toJson[Percentage](percentage)))
      case (fact, bigDecimal: BigDecimal) :: tail => inner(tail, js + (fact.name -> Json.toJson[BigDecimal](bigDecimal)))
      case _ => throw new IllegalArgumentException
    }
    inner(context.toList, JsObject(Map.empty[String, JsValue]))
  }

}
