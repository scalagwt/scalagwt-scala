class PatternMatching {

  def simplePattern(xs: List[String]) {
    xs match {
      case Nil => println("empty list")
      case x :: Nil => println("singleton list")
      case xs => println("more than one element list")
    }
  }

}