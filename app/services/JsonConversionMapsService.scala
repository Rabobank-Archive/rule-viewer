package services

import javax.inject.{Inject, Singleton}

import controllers.conversion.ConvertToFunc
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
    override def jsonToFactConversions: Map[String, ConvertToFunc] = DefaultJsonConversion.jsonToFactConversions ++ jsonConversionMaps.flatMap{
      case (_, map: JsonConversionsProvider) => map.jsonToFactConversions
    }

    override def contextToJsonConversions: Map[String, (Fact[Any], Any) => JsObject] = DefaultJsonConversion.contextToJsonConversions ++ jsonConversionMaps.flatMap{
      case (_, map: JsonConversionsProvider) => map.contextToJsonConversions
    }
  }

}
