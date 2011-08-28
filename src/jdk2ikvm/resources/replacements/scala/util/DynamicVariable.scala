/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.util

/** `DynamicVariables` provide a binding mechanism where the current
 *  value is found through dynamic scope, but where access to the
 *  variable itself is resolved through static scope.
 *
 *  The current value can be retrieved with the value method. New values
 *  should be pushed using the `withValue` method. Values pushed via
 *  `withValue` only stay valid while the `withValue`'s second argument, a
 *  parameterless closure, executes. When the second argument finishes,
 *  the variable reverts to the previous value.
 *
 *  {{{
 *  someDynamicVariable.withValue(newValue) {
 *    // ... code called in here that calls value ...
 *    // ... will be given back the newValue ...
 *  }
 *  }}}
 *
 *  Since JS is single-threaded, this version stores the binding in an ordinary variable.
 *
 *  @author  Aaron Novstrup
 */
class DynamicVariable[T](init: T) {
  var value = init

  /** Set the value of the variable while executing the specified
    * thunk.
    *
    * @param newval The value to which to set the variable
    * @param thunk The code to evaluate under the new setting
    */
  def withValue[S](newval: T)(thunk: => S): S = {
    val oldval = value
    value = newval

    try thunk
    finally value = oldval
  }

  override def toString: String = "DynamicVariable(" + value + ")"
}
