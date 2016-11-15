package controllers.conversion

import org.scalarules.finance.nl._
import play.api.data.validation.ValidationError
import play.api.libs.json._

object ImplicitConversions {
  implicit object bedragFormat extends Format[Bedrag] {
    def writes(bedrag: Bedrag): JsValue = {
      Json.toJson(bedrag.waarde)
    }

    def reads(jsValue: JsValue): JsResult[Bedrag] = jsValue match {
      case jsNumber: JsNumber => JsSuccess(jsNumber.value.euro)
      case other: Any => JsError(ValidationError("error.invalid.bedrag", other))
    }
  }

  implicit object percentageFormat extends Format[Percentage] {
    def writes(percentage: Percentage): JsValue = {
      Json.toJson(percentage.percentage)
    }

    def reads(jsValue: JsValue): JsResult[Percentage] = jsValue match {
      case jsNumber: JsNumber => JsSuccess(jsNumber.value.procent)
      case other: Any => JsError(ValidationError("error.invalid.percentage", other))
    }
  }
}
