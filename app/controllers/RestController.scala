package controllers

import javax.inject.Inject

import controllers.conversion.{OutputConverter, InputConverter}
import org.scalarules.engine.{Context, Fact}
import play.api.mvc.{Action, Controller}
import services.{DerivationsService, GlossariesService}

// scalastyle:off public.methods.have.type

class RestController @Inject() (derivationsService: DerivationsService, glossariesService: GlossariesService) extends Controller {

  def runAll = Action(parse.json) { request =>
      val (initialContext: Context, missingFacts: List[String]) = InputConverter.convertToFactsAndAddToContext(request.body, glossariesService.mergedGlossaries)
      //TODO: missingFacts should be passed around so they can be added to the back of the JSON

      val resultContext: Context = RulesRunner.run(initialContext, derivationsService.derivations)

      Ok (OutputConverter.buildJsonFromContext(resultContext))
  }

}
