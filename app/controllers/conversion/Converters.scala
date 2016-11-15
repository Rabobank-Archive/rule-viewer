package controllers.conversion

import org.scalarules.engine._
import org.scalarules.finance.nl._
import play.api.libs.json._

import scala.annotation.tailrec

object InputConverter {

  def convertToFactsAndAddToContext(inputJsValue: JsValue, factMap: Map[String, Fact[Any]]): (Context, List[String]) = {
    val newStuff: List[(String, JsValue)] = (inputJsValue match {
      case c: JsObject => c.fields.toMap
      case o: Any => ???
    }).toList

    @tailrec
    def findFactInGlossaryAndAddToContext(contextList: List[(String, JsValue)], context: Context, missingFacts: List[String]): (Context, List[String]) = contextList match {
      case Nil => (context, missingFacts)
      case (factName, factValue) :: tail => factMap.get(factName) match {
        case Some(fact) => findFactInGlossaryAndAddToContext(tail, context ++ convertFactToContext(fact, factValue), missingFacts)
        case None => findFactInGlossaryAndAddToContext(tail, context, factName :: missingFacts)
      }
    }

    def convertFactToContext(fact: Fact[Any], factValue: JsValue): Context = (fact.valueType, factValue) match {
      case ("String", factValue: JsString) => Map(fact -> factValue.value)
      case ("org.scalarules.finance.nl.Bedrag", factValue: JsValue) if Json.fromJson[Bedrag](factValue).isSuccess => Map(fact -> Json.fromJson[Bedrag](factValue).get)
      case ("org.scalarules.finance.nl.Percentage", factValue: JsValue) if Json.fromJson[Percentage](factValue).isSuccess => Map(fact -> Json.fromJson[Percentage](factValue).get)
      case ("BigDecimal", factValue: JsValue) if Json.fromJson[BigDecimal](factValue).isSuccess => Map(fact -> Json.fromJson[BigDecimal](factValue).get)
      case other: Any => throw new IllegalArgumentException("Apparently we didn't implement conversion for type " + other + " or it was in an illegal format")
    }
    findFactInGlossaryAndAddToContext(newStuff, Map.empty[Fact[Any], Any], List.empty[String])
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
