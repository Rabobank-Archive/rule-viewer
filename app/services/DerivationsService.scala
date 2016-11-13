package services

import javax.inject.{Inject, Singleton}

import models.graph.Graph
import org.scalarules.dsl.nl.grammar.Berekening
import org.scalarules.engine.Derivation
import play.api.Configuration

@Singleton
class DerivationsService @Inject() (configuration: Configuration, jarLoaderService: JarLoaderService) {

  val derivations: List[Derivation] = jarLoaderService.jars.flatMap(jarEntry => {
    jarEntry._2.derivations.map( d => d.berekeningen)
  }).toList.flatten

  val derivationContainers: Map[String, Berekening] = jarLoaderService.jars.flatMap(jarEntry => {
    jarEntry._2.derivations.map( d => (d.getClass.getName, d) )
  })

  val derivationGraphs: Map[String, Graph] = derivationContainers.map {
    case (derivationName: String, loadedDerivation) => (derivationName, DerivationsToGraphModel.convert(loadedDerivation))
  }

  def findById(id: String): Option[Berekening] = derivationContainers.get(id)
  def findGraphById(id: String): Option[Graph] = derivationGraphs.get(id)

}
