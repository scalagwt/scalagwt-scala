/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.runtime

import java.lang.Character

final class RichChar(val self: Char) extends IntegralProxy[Char] {
  def asDigit: Int                      = Character.digit(self, Character.MAX_RADIX)

  def isDigit: Boolean                  = Character.isDigit(self)
  def isLetter: Boolean                 = Character.isLetter(self)
  def isLetterOrDigit: Boolean          = Character.isLetterOrDigit(self)
  def isHighSurrogate: Boolean          = Character.isHighSurrogate(self)
  def isLowSurrogate: Boolean           = Character.isLowSurrogate(self)
  def isSurrogate: Boolean              = isHighSurrogate || isLowSurrogate

  def isLower: Boolean                  = Character.isLowerCase(self)
  def isUpper: Boolean                  = Character.isUpperCase(self)

  def toLower: Char                     = Character.toLowerCase(self)
  def toUpper: Char                     = Character.toUpperCase(self)
  
  // Java 5 Character methods not added:
  //
  // public static boolean isDefined(char ch)
  // public static boolean isJavaIdentifierStart(char ch)
  // public static boolean isJavaIdentifierPart(char ch)

  @deprecated("Use ch.toLower instead", "2.8.0")
  def toLowerCase: Char = toLower
  @deprecated("Use ch.toUpper instead", "2.8.0")
  def toUpperCase: Char = toUpper

  @deprecated("Use ch.isLower instead", "2.8.0")
  def isLowerCase: Boolean = isLower
  @deprecated("Use ch.isUpper instead", "2.8.0")
  def isUpperCase: Boolean = isUpper
}
