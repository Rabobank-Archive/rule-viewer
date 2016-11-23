import org.scalarules.engine.Fact
import play.api.libs.json._

package object controllers {
  type ConvertFunc = (Fact[Any], JsValue) => JsResult[Any]
  type ConvertBackFunc = (Fact[Any], Any) => JsObject
}
