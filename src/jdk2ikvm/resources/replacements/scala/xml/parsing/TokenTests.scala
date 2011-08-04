/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.xml
package parsing

/**
 * Helper functions for parsing XML fragments
 */
trait TokenTests {

  /** {{{
   *  (#x20 | #x9 | #xD | #xA)
   *  }}} */
  final def isSpace(ch: Char): Boolean = ch match {
    case '\u0009' | '\u000A' | '\u000D' | '\u0020' => true
    case _                                         => false
  }
  /** {{{
   *  (#x20 | #x9 | #xD | #xA)+
   *  }}} */
  final def isSpace(cs: Seq[Char]): Boolean = cs.nonEmpty && (cs forall isSpace)
  
  /** These are 99% sure to be redundant but refactoring on the safe side. */
  def isAlpha(c: Char) = (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')
  def isAlphaDigit(c: Char) = isAlpha(c) || (c >= '0' && c <= '9')

  /** {{{
   *  NameChar ::= Letter | Digit | '.' | '-' | '_' | ':' 
   *             | CombiningChar | Extender
   *  }}}
   *  See [4] and Appendix B of XML 1.0 specification.
  */
  def isNameChar(ch: Char): Boolean = {
    import java.lang.Character._
    // The constants represent groups Mc, Me, Mn, Lm, and Nd.

    sys.error("depends on RichChar.getType, not supported by GWT")
  }

  /** {{{
   *  NameStart ::= ( Letter | '_' )
   *  }}}
   *  where Letter means in one of the Unicode general 
   *  categories `{ Ll, Lu, Lo, Lt, Nl }`.
   *
   *  We do not allow a name to start with `:`.
   *  See [3] and Appendix B of XML 1.0 specification
   */
  def isNameStart(ch: Char): Boolean = {
    import java.lang.Character._
    
    sys.error("depends on RichChar.getType, not supported by GWT")
  }

  /** {{{
   *  Name ::= ( Letter | '_' ) (NameChar)*
   *  }}}
   *  See [5] of XML 1.0 specification.
   */
  def isName(s: String) =
    s.nonEmpty && isNameStart(s.head) && (s.tail forall isNameChar)

  def isPubIDChar(ch: Char): Boolean =
    isAlphaDigit(ch) || (isSpace(ch) && ch != '\u0009') ||
    ("""-\()+,./:=?;!*#@$_%""" contains ch)

  /**
   * Returns `true` if the encoding name is a valid IANA encoding.
   * This method does not verify that there is a decoder available
   * for this encoding, only that the characters are valid for an
   * IANA encoding name.
   *
   * @param ianaEncoding The IANA encoding name.
   */
  def isValidIANAEncoding(ianaEncoding: Seq[Char]) = {
    def charOK(c: Char) = isAlphaDigit(c) || ("._-" contains c)

    ianaEncoding.nonEmpty && isAlpha(ianaEncoding.head) &&
    (ianaEncoding.tail forall charOK)
  }

  def checkSysID(s: String) = List('"', '\'') exists (c => !(s contains c))
  def checkPubID(s: String) = s forall isPubIDChar
}
