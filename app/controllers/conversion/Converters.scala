package controllers.conversion

import org.scalarules.engine._
import org.scalarules.finance.nl._
import play.api.data.validation.ValidationError
import play.api.libs.json._
import controllers.conversion.ImplicitConversions._

import controllers.ConvertFunc
import scala.annotation.tailrec

object InputConverter {
  def convertToIndividualContext(inputJsValue: JsValue, factMap: Map[String, Fact[Any]]): (List[JsSuccess[Context]], List[JsError]) = {
    val jsResults: List[JsResult[Context]] = inputJsValue match {
      case c: JsObject => ImplicitConversions.factFormat.reads(c, factMap)
      case o: Any => List(JsError(ValidationError("No JsValues other than JsObject are allowed!", inputJsValue)))
    }

    val finish : List[JsSuccess[Context]] = jsResults.collect{ case succes: JsSuccess[Context] => succes }
    val him: List[JsError] = jsResults.collect{ case error: JsError => error }

    (finish, him)
  }
  def convertToFactsAndAddToContext(inputJsValue: JsValue, factMap: Map[String, Fact[Any]], conversionTypesMap: Map[String, ConvertFunc]): (Context, List[String]) = {
    val possibleFacts: List[(String, JsValue)] = (inputJsValue match {
      case c: JsObject => c.fields.toMap
      case o: Any => throw new IllegalArgumentException
    }).toList

    @tailrec
    def findFactInGlossaryAndAddToContext(possibleFacts: List[(String, JsValue)], initialContext: Context, missingFacts: List[String]): (Context, List[String]) = possibleFacts match {
      case Nil => (initialContext, missingFacts)
      case (factName, factValue) :: tail => factMap.get(factName) match {
        case Some(fact) => findFactInGlossaryAndAddToContext(tail, initialContext ++ convertFactToContext(fact, factValue), missingFacts)
        case None => findFactInGlossaryAndAddToContext(tail, initialContext, factName :: missingFacts)
      }
    }

    def convertFactToContext(fact: Fact[Any], factValue: JsValue): Context = {
      conversionTypesMap.get(fact.valueType).get(fact, factValue) match {
        case success: JsSuccess[Context] => success.get
        case failure: JsError => throw new IllegalArgumentException(s"Unable to convert fact $fact.name with value $factValue to context")
      }
    }
    findFactInGlossaryAndAddToContext(possibleFacts, Map.empty[Fact[Any], Any], List.empty[String])
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
