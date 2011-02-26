trait OuterTrait {
  private object Inner
}

object OuterObject {
  trait Inner
}

trait ShouldBeInterface {
  def test(x: Int): Int
}

class ParentForTrait {
  val x = 0
  def y = 0
}

trait ExtendsClassTrait extends ParentForTrait {
  def z: Int
}

class ExtendsTraitThatExtendsClass extends ExtendsClassTrait {
  def z = 0
}
