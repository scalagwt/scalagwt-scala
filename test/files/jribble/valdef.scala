abstract class ValDef {

  val x: Int = 0
  var y: Int = 0
  var z: Int
  
  val a: Int = 1

  def defsInMethod {
    val x: Int = 0
    var y: Int = 0
  }
  
  def valRefs {
    val a1 = a
    val b1 = ValDef.b
  }
}

object ValDef {
  val b: Int = 1
}
