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

trait WithImplementation {
  def x = 0
}

class InheritsImplementation extends WithImplementation {
  def y = 0
}
