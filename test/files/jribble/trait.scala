trait OuterTrait {
  private object Inner
}

object OuterObject {
  trait Inner
}

trait ShouldBeInterface {
  def test(x: Int): Int
}
