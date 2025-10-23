package test

import munit.FunSuite
import linearfn.LinearFnBase

/**
 * Shared test suite for all LinearFn implementations.
 * Takes an instance of LinearFnBase and runs tests on it using path-dependent types.
 */
abstract class LinearFnTestSuite(impl: LinearFnBase, name: String) extends FunSuite:

  test(s"$name: single argument - return unchanged") {
    val str = "hello"
    val result = impl.LinearFn.apply(Tuple1(str))(refs =>
      Tuple1(refs._1)
    )
    assertEquals(result, Tuple1(str))
  }

  test(s"$name: two arguments - return unchanged") {
    val str = "hello"
    val num = 42
    val result = impl.LinearFn.apply((str, num))(refs =>
      (refs._1, refs._2)
    )
    assertEquals(result, (str, num))
  }

  test(s"$name: two arguments - switch order") {
    val str = "hello"
    val num = 42
    val result = impl.LinearFn.apply((str, num))(refs =>
      (refs._2, refs._1)
    )
    assertEquals(result, (num, str))
  }
