package controllers.conversion

import controllers.ConvertFunc
import org.scalarules.engine.{Context,Fact}
import org.scalarules.finance.nl._
import play.api.data.validation.ValidationError
import play.api.libs.json._

trait ConvertMap {
  def factConversionMap: Map[String, (Fact[Any], JsValue) => JsResult[Any]]
}

object CoMap extends ConvertMap {

  override def factConversionMap: Map[String, ConvertFunc] = Map[String, ConvertFunc](
      "String" -> { stringFunct(_, _) },
      "org.scalarules.finance.nl.Bedrag" -> { bedragFunct(_, _)},
      "org.scalarules.finance.nl.Percentage" -> { percentageFunct(_, _)},
      "BigDecimal" -> { bigDecimalFunct(_, _) }
    )


  private def stringFunct(fact: Fact[Any], factValue: JsValue): JsResult[Context] = factValue match {
    case jsString: JsString => JsSuccess(Map(fact -> jsString.value))
    case _ => throw new IllegalArgumentException("String")
  }

  private def bigDecimalFunct(fact: Fact[Any], factValue: JsValue): JsResult[Context] = factValue match {
    case jsNumber: JsNumber => JsSuccess(Map(fact -> jsNumber.value))
    case _ => JsError(ValidationError(s"Conversion for BigDecimal fact ${fact.name} failed, corresponding value was not of expected type JsNumber"))
  }

  private def bedragFunct(fact: Fact[Any], factValue: JsValue): JsResult[Context] = factValue match {
    case jsNumber: JsNumber => JsSuccess(Map(fact -> jsNumber.value.euro))
    case _ => JsError(ValidationError(s"Conversion for Bedrag fact ${fact.name} failed, corresponding value was not of expected type JsNumber"))
  }

  private def percentageFunct(fact: Fact[Any], factValue: JsValue): JsResult[Context] = factValue match {
    case jsNumber: JsNumber => JsSuccess(Map(fact -> jsNumber.value.procent))
    case _ => JsError(ValidationError(s"Conversion for Percentage fact ${fact.name} failed, corresponding value was not of expected type JsNumber"))
  }
}

