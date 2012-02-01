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
    factoryManifests,          // replace regular Manifests with FactoryManifests
    removeNothingExpressions,  // move Nothing-type expressions to top level
    removeForwardJumps,        // translate forward jumps into method calls
    normalizeForJribble,       // many normalizations needed for emitting Jribble
    genJribble                 // generate .jribble files
  )

}
