package controllers.conversion

import controllers._
import org.scalarules.engine.{Context,Fact}
import org.scalarules.finance.nl._
import play.api.data.validation.ValidationError
import play.api.libs.json._
import ImplicitConversions._

trait JsonConversionMap {
  def jsonToFactConversionMap: Map[String, (Fact[Any], JsValue) => JsResult[Context]]
  def contextToJsonConversionMap: Map[String, (Fact[Any], Any) => JsObject]
}

object JsonConversion extends JsonConversionMap {
  override def contextToJsonConversionMap: Map[String, ConvertBackFunc] = ContextToJsonConversionMap.contextToJsonConversionMap
  override def jsonToFactConversionMap: Map[String, ConvertFunc] = JsonToFactConversionMap.jsonToFactConversionMap

  object ContextToJsonConversionMap {
    def contextToJsonConversionMap: Map[String, ConvertBackFunc] = Map[String, ConvertBackFunc](
      //ToDo: Make this pretty somehow!
      //Tried to do this not Stringly-typed, couldn't get it to work while still having the flexibility a Map provides regarding extension
      "String" -> { contextStringToJsObject(_, _) },
      "org.scalarules.finance.nl.Bedrag" -> { contextBedragToJsObject(_, _) },
      "org.scalarules.finance.nl.Percentage" -> { contextPercentageToJsObject(_, _) },
      "BigDecimal" -> { contextBigDecimalToJsObject(_, _) }
    )

    private def contextStringToJsObject(fact: Fact[Any], factValue: Any): JsObject = factValue match {
      case string: String => JsObject(Map(fact.name -> Json.toJson(factValue.toString)))
      case _ => throw new IllegalArgumentException
    }

    private def contextBedragToJsObject(fact: Fact[Any], factValue: Any): JsObject = factValue match {
      case bedrag: Bedrag => JsObject(Map(fact.name -> Json.toJson[Bedrag](bedrag)))
      case _ => throw new IllegalArgumentException
    }

    private def contextPercentageToJsObject(fact: Fact[Any], factValue: Any): JsObject = factValue match {
      case percentage: Percentage => JsObject(Map(fact.name -> Json.toJson[Percentage](percentage)))
      case _ => throw new IllegalArgumentException
    }

    private def contextBigDecimalToJsObject(fact: Fact[Any], factValue: Any): JsObject = factValue match {
      case bigDecimal: BigDecimal => JsObject(Map(fact.name -> Json.toJson[BigDecimal](bigDecimal)))
      case _ => throw new IllegalArgumentException
    }

  }

  object JsonToFactConversionMap {
    def jsonToFactConversionMap: Map[String, ConvertFunc] =   Map[String, ConvertFunc](
      "String" -> { stringFunct(_, _) },
      "org.scalarules.finance.nl.Bedrag" -> { bedragFunct(_, _, jsResultToContext)},
      "org.scalarules.finance.nl.Percentage" -> { percentageFunct(_, _, jsResultToContext)},
      "BigDecimal" -> { bigDecimalFunct(_, _) }
    )

    private def stringFunct(fact: Fact[Any], factValue: JsValue): JsResult[Context] = factValue match {
      case jsString: JsString => JsSuccess(Map(fact -> jsString.value))
      case _ => JsError(ValidationError(s"Conversion for String fact ${fact.name} failed, corresponding value was not of expected type JsString"))
    }

    private def bigDecimalFunct(fact: Fact[Any], factValue: JsValue): JsResult[Context] = factValue match {
      case jsNumber: JsNumber => JsSuccess(Map(fact -> jsNumber.value))
      case _ => JsError(ValidationError(s"Conversion for BigDecimal fact ${fact.name} failed, corresponding value was not of expected type JsNumber"))
    }

    private def bedragFunct(fact: Fact[Any], factValue: JsValue, f: (Fact[Any], JsResult[Any]) => JsResult[Context]): JsResult[Context] = factValue match {
      case jsNumber: JsNumber => f(fact, Json.fromJson[Bedrag](jsNumber))
      case _ => JsError(ValidationError(s"Conversion for Bedrag fact ${fact.name} failed, corresponding value was not of expected type JsNumber"))
    }

    private def percentageFunct(fact: Fact[Any], factValue: JsValue, f: (Fact[Any], JsResult[Any]) => JsResult[Context]): JsResult[Context] = factValue match {
      case jsNumber: JsNumber => f(fact, Json.fromJson[Percentage](jsNumber))
      case _ => JsError(ValidationError(s"Conversion for Percentage fact ${fact.name} failed, corresponding value was not of expected type JsNumber"))
    }

    private def jsResultToContext(fact: Fact[Any], jsResult: JsResult[Any]): JsResult[Context] = jsResult match {
      case success: JsSuccess[Any] => JsSuccess(Map(fact -> success.get))
      case error: JsError => error
    }
  }

}

