/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author  Grzegorz Kossakowski
 */

package scala.tools.nsc
package backend

import io.AbstractFile
import util.JavaClassPath
import util.ClassPath.{ JavaContext, DefaultJavaContext }
import scala.tools.util.PathResolver

trait JribblePlatform extends JavaPlatform {
  import global._

  override def platformPhases = super.platformPhases ++ List(
    flatten,                   // get rid of inner classes
    removeNothingExpressions,  // move Nothing-type expressions to top level
    removeForwardJumps,        // translate forward jumps into method calls
    normalizeForJribble,       // many normalizations needed for emitting Jribble
    genJribble                 // generate .jribble files
  ) ++ loadFactoryManifestsPlugin.components //add `factorymanifests` plugin components, they are required!
  
  def loadFactoryManifestsPlugin =
    global.plugins.find(_.name == "factorymanifests") getOrElse
      sys.error("Jribble backend requires `factorymanifests` plugin. Did you forget to include it using -Xplugin option?")
}
