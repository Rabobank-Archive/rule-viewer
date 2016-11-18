package controllers.conversion

import controllers.ConvertFunc
import org.scalarules.engine.Fact
import org.scalarules.engine.Context
import org.scalarules.finance.nl._
import play.api.data.validation.ValidationError
import play.api.libs.json._

import scala.annotation.tailrec

object ImplicitConversions {

  object factFormat extends {
    def writes(fact: Fact[Bedrag], c: Context): JsValue = {
      Json.toJson(fact.toEval.apply(c))
    }

    def reads(jsObject: JsObject, possibleFacts: Map[String, Fact[Any]]): List[JsResult[Context]] = {
      val prospectiveFacts: List[(String, JsValue)] = jsObject.fields.toList

      @tailrec
      def inner(list: List[(String, JsValue)], resultList: List[JsResult[Context]]): List[JsResult[Context]] = list match {
        case Nil => resultList
        case ((factName: String, factValue: JsValue) :: tail) => possibleFacts.get(factName) match {
          case Some(fact) => CoMap.factConversionMap.get(fact.valueType) match {
            case function: Some[ConvertFunc] => inner(tail, function.get(fact, factValue) :: resultList)
            case None => inner(tail, JsError(ValidationError(s"Unable to find suitable fromJson conversion for Fact with name $factName with type ${fact.valueType} in factConversionMap", factValue)) :: resultList)
          }
          case None => inner(tail, JsError(ValidationError(s"Unable to find Fact with name $factName in the Glossary", factValue)) :: resultList)
        }
        case _ => JsError(ValidationError("Something weird has happened during conversion")) :: resultList
      }
      inner(prospectiveFacts, List.empty)
    }
  }

  implicit object bedragWrites extends Writes[Bedrag] {
    def writes(bedrag: Bedrag): JsValue = {
      Json.toJson(bedrag.waarde)
    }
  }

  implicit object percentageWrites extends Writes[Percentage] {
    def writes(percentage: Percentage): JsValue = {
      Json.toJson(percentage.percentage)
    }
  }
}
