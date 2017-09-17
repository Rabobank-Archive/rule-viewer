package services

import javax.inject.{Inject, Singleton}

import models.graph.Graph
import nl.rabobank.rules.dsl.nl.grammar.Berekening
import play.api.Configuration

@Singleton
class DerivationsService @Inject() (configuration: Configuration, jarLoaderService: JarLoaderService) {

  val derivations: Map[String, Berekening] = jarLoaderService.jars.flatMap( jarEntry => {
    jarEntry._2.derivations.map( d => (d.getClass.getName, d) )
  })

  val derivationGraphs: Map[String, Graph] = derivations.map {
    case (derivationName: String, loadedDerivation) => (derivationName, DerivationsToGraphModel.convert(loadedDerivation))
  }

  def findById(id: String): Option[Berekening] = derivations.get(id)
  def findGraphById(id: String): Option[Graph] = derivationGraphs.get(id)

}
