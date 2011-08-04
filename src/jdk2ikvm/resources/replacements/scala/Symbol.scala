/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala

/** This class provides a simple way to get unique objects for equal strings.
 *  Since symbols are interned, they can be compared using reference equality.
 *  Instances of `Symbol` can be created easily with Scala's built-in quote
 *  mechanism.
 *  
 *  For instance, the [[http://scala-lang.org/#_top Scala]] term `'mysym` will
 *  invoke the constructor of the `Symbol` class in the following way:
 *  `Symbol("mysym")`.
 *  
 *  @author  Martin Odersky, Iulian Dragos
 *  @version 1.8
 */
final class Symbol private (val name: String) extends Serializable {
  /** Converts this symbol to a string.
   */
  override def toString(): String = "'" + name

  private def readResolve(): Any = Symbol.apply(name)
}

object Symbol extends UniquenessCache[String, Symbol]
{
  protected def valueFromKey(name: String): Symbol = new Symbol(name)
  protected def keyFromValue(sym: Symbol): Option[String] = Some(sym.name)
}

/** This is private so it won't appear in the library API, but
  * abstracted to offer some hope of reusability.  */
private[scala] abstract class UniquenessCache[K, V >: Null]
{
  private val map = new java.util.HashMap[K, V]

  protected def valueFromKey(k: K): V
  protected def keyFromValue(v: V): Option[K]

  def apply(name: K): V = {
    def cached(): V = {
      map get name
    }
    def updateCache(): V = {
      val res = cached()
      if (res != null) res else {
        val sym = valueFromKey(name)
        map.put(name, sym)
        sym
      }
    }

    val res = cached()
    if (res == null) updateCache()
    else res
  }
  def unapply(other: V): Option[K] = keyFromValue(other)
}
