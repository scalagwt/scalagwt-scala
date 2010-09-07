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
  
  //TODO(grek): Disabled test due to bug in if expression handling
  /**def ifShouldBeTransformerIntoCondtional = 
    if (cond)
      "Truly"
    else
      "Fully?"**/
      
  def ifWithStatmentsButReturningValue = {
    if (cond) {
      cond
      0
    } else {
      cond
      1
    }
  }
  
  //TODO(grek): Disabled test due to bug in if expression handling
  /**def ifWithoutElseAsExpr {
    //here else branch will be implicitly added with () expression inside
    //then Unit is being boxed so if can act as an expression
    val x = if (cond) {
      0
    }
  }**/
  
  def ifWithoutElseAsStmt {
    if (cond) {
      0
    }
  }
  
  def sideeffect: Unit = ()
  
  //the purpose of this test is to check if jribble backend can handle deeply nested if construct
  //it used to have O(2^n) complexity algorithm for handling nested if expressions. By having deeply
  //nested if expressions we can check if it compiles in reasonable time and make sure that there is no
  //exponential complexity lurking
  def deeplyNestedIf {
    val x = 0
    if (x == 0) {
      sideeffect
    } else if (x == 1) {
      sideeffect
    } else if (x == 2) {
      sideeffect
    } else if (x == 3) {
      sideeffect
    } else if (x == 4) {
      sideeffect
    } else if (x == 5) {
      sideeffect
    } else if (x == 6) {
      sideeffect
    } else if (x == 7) {
      sideeffect
    } else if (x == 8) {
      sideeffect
    } else if (x == 9) {
      sideeffect
    } else if (x == 10) {
      sideeffect
    } else if (x == 11) {
      sideeffect
    } else if (x == 12) {
      sideeffect
    } else if (x == 13) {
      sideeffect
    }
  }
}
