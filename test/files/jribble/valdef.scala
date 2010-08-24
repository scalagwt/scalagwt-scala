abstract class ValDef {

  val x: Int = 0
  var y: Int = 0
  var z: Int

  def defsInMethod {
    val x: Int = 0
    var y: Int = 0
  }
}
