package scala

package object reflect {
  // !!! This was a val; we can't throw exceptions that aggressively without breaking
  // non-standard environments, e.g. google app engine.  I made it a lazy val, but
  // I think it would be better yet to throw the exception somewhere else - not during
  // initialization, but in response to a doomed attempt to utilize it.
  lazy val mirror: api.Mirror = {
    throw new UnsupportedOperationException("Scala reflection not available on this platform")
  }

  /** Uncomment once we got rid of the old Symbols, Types, Trees
  type Symbol = mirror.Symbol
  type Type = mirror.Type
  type Tree = mirror.Tree
  */
}
