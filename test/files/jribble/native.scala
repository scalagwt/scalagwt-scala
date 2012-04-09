//test @native in object

object A {
  @native
  def foo(x: String) = x
}

//test @native in class

class B {
  @native
  def foo(x: String) = x
}

//test @native in trait

trait C {
  @native
  def foo(x: String) = x
}

//test @native in companion pair

object D {
  @native
  def foo(x: String) = x
}

class D {
  @native
  def bar(x: Int) = x
}
