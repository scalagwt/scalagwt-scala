class Constants {
  def chars = {
    val minChar = Char.MinValue
    val maxChar = Char.MaxValue
    //should be translated into \t
    val tabChar = '\011'
  }
  
  def strings = {
    val string = "\011"
  }
  
  def doubles {
    val min = -java.lang.Double.MAX_VALUE
    val eps = java.lang.Double.MIN_VALUE
    val max = java.lang.Double.MAX_VALUE
    val nan = java.lang.Double.NaN
    val negInf = java.lang.Double.NEGATIVE_INFINITY
    val posInf = java.lang.Double.POSITIVE_INFINITY
    val zero = 0.0d
    val minusZero = -0.0d
  }
  
  def floats {
    val min  = -java.lang.Float.MAX_VALUE
    val eps  = java.lang.Float.MIN_VALUE
    val max  = java.lang.Float.MAX_VALUE
    val nan  = java.lang.Float.NaN
    val negInf = java.lang.Float.NEGATIVE_INFINITY
    val posInf = java.lang.Float.POSITIVE_INFINITY
    val zero = 0.0f
    val minusZero = -0.0f
  }
}
