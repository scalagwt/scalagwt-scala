class Test {

  def testCaseOr = { true || true }
  def testCaseAnd = { false && true }
  def testCaseOrComplex = { callA || callB || callC }
  def testCaseAndComplex = { callA && callB && callC }

  def callA = true
  def callB = false
  def callC = true

}
