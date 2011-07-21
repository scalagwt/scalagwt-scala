class Cast {

  def isInstanceOfCheckOnAnyRef {
    val x: AnyRef = new Object
    val isString = x.isInstanceOf[String]
    //should result in instanceof check using java.lang.Byte
    val isByte = x.isInstanceOf[Byte]
    //should result in instanceof check using scala.runtime.BoxedUnit
    val isUnit = x.isInstanceOf[Unit]
  }
  
  def asInstanceOfCastFromAny {
    val x: Any = new Object
    val y = x.asInstanceOf[String]
    //should result in unboxing to byte using BoxesRunTime.unboxToByte
    val b = x.asInstanceOf[Byte]
    //should result in casting using boxed type scala.runtime.BoxedUnit
    val u = x.asInstanceOf[Unit]
  }

  def asInstanceOfCastFromInt {
    val x: Int = 0
    //should result in boxing and then casting to String
    val y = x.asInstanceOf[String]
    //should result in casting using primitive type byte
    val b = x.asInstanceOf[Byte]
    //should result in boxing and then casting to scala.runtime.BoxedUnit
    val u = x.asInstanceOf[Unit]
  }

}
