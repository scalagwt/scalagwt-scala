class Test {

  def testCaseOr = { true || { shouldNotCall(); true } }
  def testCaseAnd = { false && { shouldNotCall(); true } }
  def testCaseOrComplex = { { callA(); true } || { callB(); false } || { callC(); false} }
  def testCaseAndComplex = { { callA(); false } && { callB(); false } && { callC(); false} }

  def shouldNotCall() = {}
  def callA() = {}
  def callB() = {}
  def callC() = {}

}
