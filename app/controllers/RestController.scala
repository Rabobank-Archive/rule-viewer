package controllers

import javax.inject.Inject

import org.scalarules.engine.{FactEngine, Context, Fact}
import org.scalarules.finance.nl._
import play.api.libs.json._
import play.api.mvc.{Action, Controller}
import services.{DerivationsService, GlossariesService}

import scala.annotation.tailrec

// scalastyle:off public.methods.have.type

class RestController @Inject() (derivationsService: DerivationsService, glossariesService: GlossariesService) extends Controller {

  def runAll = Action(parse.json) {
    request => Ok (startCalculation (request.body) )
  }

  def startCalculation(inputJsValue: JsValue): JsValue = {
    val (initialContext: Context, missingFacts : List[String]) = convertToScalaAndAddToContext(inputJsValue)
    val resultContext: Context = run(initialContext)
    buildJsFromContext(resultContext)
  }

  def convertToScalaAndAddToContext(inputJsValue: JsValue): (Context, List[String]) = {
    val factMap: Map[String, Fact[Any]] = glossariesService.mergedGlossaries
    val newStuff: List[(String, JsValue)] = (inputJsValue match {
      case c: JsObject => c.fields.toMap
      case o: Any => ???
    }).toList

    @tailrec
    def findFactInGlossaryAndAddToContext(contextList: List[(String, JsValue)], context: Context, missingFacts: List[String]) : (Context, List[String]) = contextList match {
      case Nil => (context, missingFacts)
      case (factName, factValue) :: tail => factMap.get(factName) match {
        case Some(fact) => findFactInGlossaryAndAddToContext(tail, context ++ convertFactToContext(fact, factValue), missingFacts)
        case None => findFactInGlossaryAndAddToContext(tail, context, factName :: missingFacts)
      }
    }

    def convertFactToContext(fact: Fact[Any], factValue: JsValue): Context = (fact.valueType, factValue) match {
      case ("String", factValue: JsString) => Map(fact -> factValue.value)
      case ("org.scalarules.finance.nl.Bedrag", factValue: JsValue) => Map(fact -> BedragConverter.fromJson(factValue))
      case ("org.scalarules.finance.nl.Percentage", factValue: JsValue) => Map(fact -> PercentageConverter.fromJson(factValue))
      case ("BigDecimal", factValue: JsValue) => Map(fact -> BigDecimalConverter.fromJson(factValue))
      case other: Any => throw new IllegalArgumentException("Apparently we didn't implement conversion for type " + other)
    }
    findFactInGlossaryAndAddToContext(newStuff, Map.empty[Fact[Any], Any], List.empty[String])
  }

  def run(context: Context): Context = {
    FactEngine.runNormalDerivations(context, derivationsService.derivations)
  }

  def makeOutputContext(inputFacts: List[(String, Any)]): Context = {
    val factMap: Map[String, Fact[Any]] = glossariesService.mergedGlossaries
    addFactsToContext(inputFacts, Map.empty[Fact[Any], Any], factMap)
  }

  def buildJsFromContext(context: Context) : JsValue = {
    @tailrec
    def inner(list: List[(Fact[Any], Any)], js: JsObject): JsValue = list match {
      case Nil => js
      case (fact, string: String) :: tail => inner(tail, js + (fact.name -> Json.toJson(string.toString)))
      case (fact, bedrag: Bedrag) :: tail => inner(tail, js + (fact.name -> BedragConverter.toJson(bedrag)))
      case (fact, percentage: Percentage) :: tail => inner(tail, js + (fact.name -> PercentageConverter.toJson(percentage)))
      case (fact, bigDecimal: BigDecimal) :: tail => inner(tail, js + (fact.name -> BigDecimalConverter.toJson(bigDecimal)))
      case _ => throw new IllegalArgumentException
      }
    inner(context.toList, JsObject(Map.empty[String, JsValue]))
  }

  def addFactsToContext(inputFacts: List[(String, Any)], context: Context, factMap: Map[String, Fact[Any]]): Context = {
    @tailrec
    def addFactToContext(inputFacts: List[(String, Any)], context: Context) : Context = inputFacts match {
      case Nil => context
      case head :: tail => factMap.get(head._1) match {
                                                      case Some(fact: Fact[Any]) => addFactToContext(tail, context ++ Map(fact -> head._2))
                                                      case None => context
                                                      case _ => throw new IllegalArgumentException
                                                    }
      case _ => throw new IllegalArgumentException
    }
    addFactToContext(inputFacts, context)
  }

  def toBedrag(fact: Fact[Any], eval: Bedrag): JsObject = {
    Json.obj(fact.name -> JsNumber(eval.waarde))
  }

}

object BedragConverter {
  def toJson(bedrag: Bedrag) : JsValue = {
    Json.toJson(bedrag.waarde)
  }
  def fromJson(jsValue: JsValue) : Bedrag = jsValue match {
    case jsNumber: JsNumber => jsNumber.value.euro
    case _ => throw new IllegalArgumentException("conversion for jsValue of type " + jsValue.getClass + " to Bedrag has not yet been implemented")
  }
}

object PercentageConverter {
  def toJson(percentage: Percentage) : JsValue = {
    Json.toJson(percentage.percentage)
  }

  def fromJson(jsValue: JsValue) : Percentage = jsValue match {
    case jsNumber: JsNumber => jsNumber.value.procent
    case _ => throw new IllegalArgumentException("conversion for jsValue of type " + jsValue.getClass + " to Percentage has not yet been implemented")
  }
}

object BigDecimalConverter {
  def toJson(bigDecimal: BigDecimal) : JsValue = {
    Json.toJson(bigDecimal)
  }

  def fromJson(jsValue: JsValue) : BigDecimal = jsValue match {
    case jsNumber: JsNumber => jsNumber.value
    case _ => throw new IllegalArgumentException("conversion for jsValue of type " + jsValue.getClass + " to BigDecimal has not yet been implemented")
  }
}


