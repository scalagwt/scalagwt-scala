class ArrayValue {

  def values {
    //it would be probably more obvious to use Array instead of List here
    //but it turns out that Array construction in Scala is far from trivial
    //and it's mechanics would hide the real purpose of this test
    //on the other hand, here we'll get List.apply(/**array literal for 0 and 1**/)
    //which is exactly what we are looking for
    val x = List(0,1)
  }

}
