package controllers

import javax.inject.Inject

import controllers.conversion.{CoMap, InputConverter, OutputConverter}
import org.scalarules.engine.{Fact, Context}
import play.api.libs.json.{JsError, JsSuccess}
import play.api.mvc.{Action, Controller}
import services.{DerivationsService, GlossariesService}

// scalastyle:off public.methods.have.type

class RestController @Inject() (derivationsService: DerivationsService, glossariesService: GlossariesService) extends Controller {

  def runAll = Action(parse.json) { request =>
      val (initialContextFragments: List[JsSuccess[Context]], conversionErrors: List[JsError]) = InputConverter.convertToIndividualContext(request.body, glossariesService.mergedGlossaries)
      //TODO: conversionErrors should be passed around so they can be added to the back of the JSON

      val initialContext: Context = initialContextFragments.foldLeft(Map.empty[Fact[Any], Any])((acc, jsSuccess) => acc ++ jsSuccess.get)
      val resultContext: Context = RulesRunner.run(initialContext, derivationsService.derivations)

      Ok (OutputConverter.buildJsonFromContext(resultContext))
  }

}

