package services

import javax.inject.{Inject, Singleton}

import controllers.ConvertFunc
import controllers.conversion.{DefaultJsonConversion, JsonConversionsProvider}
import org.scalarules.engine._
import play.api.Configuration
import play.api.libs.json.JsObject

@Singleton
class JsonConversionMapsService @Inject()(configuration: Configuration, jarLoaderService: JarLoaderService) {

  val jsonConversionMaps: Map[String, JsonConversionsProvider] = jarLoaderService.jars.flatMap(jarEntry => {
    jarEntry._2.jsonConversionsProviders.map(g => (g.getClass.getName, g) )
  })

  val mergedJsonConversionMap: JsonConversionsProvider = new JsonConversionsProvider {
    override def jsonToFactConversions: Map[String, ConvertFunc] = DefaultJsonConversion.jsonToFactConversions ++ jsonConversionMaps.flatMap{
      case (_, map: JsonConversionsProvider) => map.jsonToFactConversions
    }

    override def contextToJsonConversions: Map[String, (Fact[Any], Any) => JsObject] = DefaultJsonConversion.contextToJsonConversions ++ jsonConversionMaps.flatMap{
      case (_, map: JsonConversionsProvider) => map.contextToJsonConversions
    }
  }

}
