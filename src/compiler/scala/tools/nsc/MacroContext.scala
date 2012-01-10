package scala.tools.nsc

import symtab.Flags._

trait MacroContext extends reflect.api.MacroContext { self: Global =>
  
  def captureVariable(vble: Symbol): Unit = vble setFlag CAPTURED
  
  def referenceCapturedVariable(id: Ident): Tree = ReferenceToBoxed(id)
}
