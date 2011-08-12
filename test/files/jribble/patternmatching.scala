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
  
  def throwInPatternBranch(x: Int): Int = x match {
      //having a guard expression triggers pattern matching to emit a tree
      //of slightly different shape that would result with assignment to
      //a variable with throw on rhs which is invalid jribble construct
      case x if x == 0 => throw new RuntimeException("don't like zeros")
      case x => x
    }

}
