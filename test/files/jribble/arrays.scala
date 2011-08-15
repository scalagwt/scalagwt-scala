class Arrays {

  def values {
    //it would be probably more obvious to use Array instead of List here
    //but it turns out that Array construction in Scala is far from trivial
    //and it's mechanics would hide the real purpose of this test
    //on the other hand, here we'll get List.apply(/**array literal for 0 and 1**/)
    //which is exactly what we are looking for
    val x = List(0,1)
  }
  
  def newArray {
    //this tests a case where we have two dimensional array but we give the constructor one
    //dimension only
    val x = new Array[Array[Object]](1)
  }
  
  def returnOneArray[T: ClassManifest]: Array[T] = Array.ofDim(1)
  
  def getOneArray[T: ClassManifest] = returnOneArray
  
  def returnTwoArray[T: ClassManifest]: Array[Array[T]] = Array.ofDim(1, 1)
  
  def getTwoArray[T: ClassManifest] = returnTwoArray
  
  def arrayLength(x: Array[Int]) = x.length
  
  def genericArrays(): Unit = {
    returnOneArray[Int]
    returnOneArray[String]
    returnOneArray[Object]
    returnOneArray[Array[String]]
    returnOneArray[Array[Array[String]]]
    returnOneArray[Array[Array[Int]]]
  }

}
