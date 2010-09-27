class PatternMatching {

  def simplePattern(xs: List[String]) {
    xs match {
      case Nil => println("empty list")
      case x :: Nil => println("singleton list")
      case xs => println("more than one element list")
    }
  }
  
  def shouldBeTranslatedToSwitch(x: Int) {
    val msg = x match {
      case 0 => "got zero"
      case 1 | 2 => "got one or two"
      case _ => "got something else"
    }
  }
  
  def unpackTuple(t: (Int, Int)) = {
    val (x, y) = t
  }

}
