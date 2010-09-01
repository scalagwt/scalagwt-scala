class Try {

  def simpleTry {
    try {
      0
    } catch {
      case x: Exception if x.getMessage == "test" => println("first case " + x)
      case x: Exception => println("second case " + x)
    }
  }
  
  def typedWildcardTry {
    try { true } catch { case _: ClassCastException => false }
  }
  
  def wildcardTry {
    try { true } catch { case _ => false }
  }
}
