/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.runtime

final class RichLong(val self: Long) extends IntegralProxy[Long] {
  def toBinaryString: String = java.lang.Long.toBinaryString(self)
  def toHexString: String = java.lang.Long.toHexString(self)
  def toOctalString: String = java.lang.Long.toOctalString(self)
}
