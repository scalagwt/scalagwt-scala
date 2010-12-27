class Foo {
  val referCompanionVal1 = Foo.x
  def referCompanionVal2 = Foo.x
  val referCompanionDef1 = Foo.y
  def referCompanionDef2 = Foo.y
}

object Foo {
  val x = 1
  def y = 2
}
