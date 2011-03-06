/** 
 * Tests whether static fields and methods defined in Java classes
 * are being handled correctly by jribble.
 */
class StaticRef {
  
  def staticField = scala.runtime.BoxedUnit.UNIT
  
  def staticMethod = scala.runtime.BoxesRunTime.boxToBoolean(false)
  
  /** 
   * This constructs triggers special construct in Scala's AST whic is
   * This node not referring to an enclosing class (StaticRef in this case).
   * It refers to some other class, java.lang.Byte in this case, so fragment
   * of the code below looks like this:
   * if (Byte.this.TYPE.==(temp1)) {
   *         0
   * }
   * We need special treatment for such construct.
   */
  def patternMatchOnStatic[T](clazz: Predef.Class[T]) = clazz match {
    case java.lang.Byte.TYPE => 0
    case _ => 1
  }
  
}
