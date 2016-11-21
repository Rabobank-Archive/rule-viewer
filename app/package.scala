import org.scalarules.engine.{Context,Fact}
import play.api.libs.json._

package object controllers {
  type ConvertFunc = (Fact[Any], JsValue) => JsResult[Context]
  type ConvertBackFunc = (Fact[Any], Any) => JsObject
}
