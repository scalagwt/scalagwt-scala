class If {

  //dummy method that is used as a condition in if and it's not
  //being optimized away
  def cond = true

  def ifWithUnboxedUnitType {
    if (cond)
      println("Hello, world!")
    else
      println("Hello odd world!")
  }
  
  def ifWithNothingType {
    if (cond)
      error("Hello, world!")
    else
      error("Hello odd world!")
  }
  
  def ifShouldBeTransformerIntoCondtional = 
    if (cond)
      "Truly"
    else
      "Fully?"
      
  def ifWithStatmentsButReturningValue = {
    if (cond) {
      cond
      0
    } else {
      cond
      1
    }
  }
  
  def ifWithoutElseAsExpr {
    //here else branch will be implicitly added with () expression inside
    //then Unit is being boxed so if can act as an expression
    val x = if (cond) {
      0
    }
  }
  
  def ifWithoutElseAsStmt {
    if (cond) {
      0
    }
  }
}
