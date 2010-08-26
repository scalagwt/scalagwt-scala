class Try {

  def simpleTry {
    try {
      0
    } catch {
      case x: Exception if x.getMessage == "test" => println("first case " + x)
      case x: Exception => println("second case " + x)
    }
  }
}
