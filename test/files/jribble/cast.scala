class Cast {

  def isInstanceOfCheck {
    val x = new Object
    val isString = x.isInstanceOf[String]
  }
  
  def asInstanceOfCast {
    val x = new Object
    val y = x.asInstanceOf[String]
  }

}
