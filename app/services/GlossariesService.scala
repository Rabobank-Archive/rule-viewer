package services

import javax.inject.{Inject, Singleton}

import nl.rabobank.oss.rules.dsl.core.glossaries.Glossary
import play.api.Configuration

@Singleton
class GlossariesService @Inject()(configuration: Configuration, jarLoaderService: JarLoaderService) {

  val glossaries: Map[String, Glossary] = jarLoaderService.jars.flatMap( jarEntry => {
    jarEntry._2.glossaries.map( g => (g.getClass.getName, g) )
  })

  def findById(id: String): Option[Glossary] = glossaries.get(id)

}
