class Operators {

  def zero = 0
  def one = 1

  def equalsOp {
    val s1 = "test"
    val s2 = "test2"
    val x1 = s1 == s2
    val x2 = zero == one
    val x3 = 0 == s1
    val x4 = s1 == 0
    val x5 = (new Object) == (new Object)
    val x6 = s1 == (new Object)
    val x7 = 0 == null
    val x8 = null == 0
    val x9 = s1 == null
    val x10 = null == s1
  }
  
  def eqOp {
    val s1 = "test"
    val s2 = "test2"
    val x1 = s1 eq s2
    val x2 = s1 ne s2
  }

}
