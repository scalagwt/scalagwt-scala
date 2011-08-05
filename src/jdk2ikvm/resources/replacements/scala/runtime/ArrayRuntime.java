/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.runtime;

/**
 * Methods on Java arrays
 */
class ArrayRuntime {
  static boolean[] cloneArray(boolean[] array) { throw new RuntimeException("array.clone"); }
  static byte[] cloneArray(byte[] array) { throw new RuntimeException("array.clone"); }
  static short[] cloneArray(short[] array) { throw new RuntimeException("array.clone"); }
  static char[] cloneArray(char[] array) { throw new RuntimeException("array.clone"); }
  static int[] cloneArray(int[] array) { throw new RuntimeException("array.clone"); }
  static long[] cloneArray(long[] array) { throw new RuntimeException("array.clone"); }
  static float[] cloneArray(float[] array) { throw new RuntimeException("array.clone"); }
  static double[] cloneArray(double[] array) { throw new RuntimeException("array.clone"); }
  static Object[] cloneArray(Object[] array) { throw new RuntimeException("array.clone"); }
}
