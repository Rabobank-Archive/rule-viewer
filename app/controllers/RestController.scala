package controllers

import javax.inject.Inject

import controllers.conversion.InputConverter
import org.scalarules.engine.{Fact, Context}
import play.api.libs.json.{Json, JsError, JsSuccess}
import play.api.mvc.{Action, Controller}
import services.{DerivationsService, GlossariesService}
import controllers.conversion.ImplicitConversions._

// scalastyle:off public.methods.have.type

class RestController @Inject() (derivationsService: DerivationsService, glossariesService: GlossariesService) extends Controller {

  def runAll = Action(parse.json) { request =>
    //ToDo: figure out how to get the glossary info to the reads without parsing it ourselves, so we can use Json.toJson
    val (initialContextFragments: List[JsSuccess[Context]], conversionErrors: List[JsError]) = InputConverter.convertToIndividualContext(request.body, glossariesService.mergedGlossaries)

    if(conversionErrors != List.empty) BadRequest(JsError.toJson(conversionErrors.reduceLeft(_ ++ _)))
    else {
      val initialContext: Context = initialContextFragments.foldLeft(Map.empty[Fact[Any], Any])((acc, jsSuccess) => acc ++ jsSuccess.get)
      val resultContext: Context = RulesRunner.run(initialContext, derivationsService.derivations)
      Ok(Json.toJson[Context](resultContext))
    }
  }

}

