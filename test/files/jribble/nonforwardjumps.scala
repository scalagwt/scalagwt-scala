class NonForwardJumps {

  def fact(n: Int): Int = {
    @scala.annotation.tailrec def tailfact(n: Int, res: Int): Int = if (n == 0) res else tailfact(n-1, res*n)
    tailfact(n, 1)
  }

}
