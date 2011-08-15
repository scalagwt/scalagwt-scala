
package scala.tools.factorymanifests

import scala.tools.nsc
import scala.tools.nsc.typechecker._
import nsc.Global
import nsc.Phase
import nsc.plugins.Plugin
import nsc.plugins.PluginComponent

class FactoryManifestsPlugin(val global: Global) extends Plugin {
  import global._

  val name = "factorymanifests"
  val description = "Implements manifests without reflection by generating anonymous classes"
  
  val phase = new FactoryManifestsTransform() { 
    val global = FactoryManifestsPlugin.this.global
    val runsAfter = List("refchecks")
  }
  
  
  val components = List[PluginComponent](phase)

  global.log("instantiated factorymanifests plugin: " + this)
}
