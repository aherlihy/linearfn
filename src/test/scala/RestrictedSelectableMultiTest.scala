package test

import linearfn.RestrictedSelectable

/**
 * Comprehensive tests for applyMulti and applyConsumedMulti.
 * Tests arbitrary number of return values with linearity constraints:
 * - All arguments must appear at least once across all returns
 * - No argument can appear more than once in any single return
 */
class RestrictedSelectableMultiTest extends LinearFnTestSuite(RestrictedSelectable, "RestrictedSelectableMulti"):

  // =========================
  // 1 Return Value Tests
  // =========================

  test("applyMulti: 1 return with 2 args - missing arg fails") {
    val obtained = compileErrors("""
      val str = "hello"
      val num = 42
      RestrictedSelectable.LinearFn.applyMulti((str, num))(refs =>
        Tuple1(refs._1)  // Missing refs._2
      )
    """)
    assert(obtained.contains(TestUtils.linearMsg), s"obtained: $obtained")
  }

  test("applyMulti: 1 return with 2 args - both args used fails (duplicate in single return)") {
    val obtained = compileErrors("""
      case class Combined(s: String, n: Int)
      val str = "hello"
      val num = 42
      RestrictedSelectable.LinearFn.applyMulti((str, num))(refs =>
        // Both args used in single return - violates affine per-return constraint
        val combined = RestrictedSelectable.Restricted.LinearRef[Combined, (0, 1), EmptyTuple](() => Combined("test", 1))
        Tuple1(combined)
      )
    """)
    assert(obtained.contains(TestUtils.restrictedTypesMsg), s"obtained: $obtained")
  }

  // =========================
  // Fewer Returns Than Arguments Tests
  // =========================

  test("applyMulti: 2 returns with 3 args - missing arg fails") {
    val obtained = compileErrors("""
      case class Data(value: Int)
      val a = Data(1)
      val b = Data(2)
      val c = Data(3)

      RestrictedSelectable.LinearFn.applyMulti((a, b, c))(refs =>
        (refs._1, refs._2)  // Missing refs._3
      )
    """)
    assert(obtained.contains(TestUtils.linearMsg), s"obtained: $obtained")
  }

  test("applyMulti: 2 returns with 3 args - duplicate arg fails") {
    val obtained = compileErrors("""
      case class Data(value: Int)
      val a = Data(1)
      val b = Data(2)
      val c = Data(3)

      RestrictedSelectable.LinearFn.applyMulti((a, b, c))(refs =>
        (refs._1, refs._1)  // Missing refs._2 and refs._3
      )
    """)
    assert(obtained.contains(TestUtils.linearMsg), s"obtained: $obtained")
  }

  // =========================
  // Same Returns As Arguments Tests
  // =========================

  test("applyMulti: 2 returns with 2 args - valid") {
    case class Data(value: Int)
    val a = Data(1)
    val b = Data(2)

    val result = RestrictedSelectable.LinearFn.applyMulti((a, b))(refs =>
      (refs._1, refs._2)
    )

    assertEquals(result._1.value, 1)
    assertEquals(result._2.value, 2)
  }

  test("applyMulti: 3 returns with 3 args - valid") {
    case class Data(value: Int)
    val a = Data(1)
    val b = Data(2)
    val c = Data(3)

    val result = RestrictedSelectable.LinearFn.applyMulti((a, b, c))(refs =>
      (refs._1, refs._2, refs._3)
    )

    assertEquals(result._1.value, 1)
    assertEquals(result._2.value, 2)
    assertEquals(result._3.value, 3)
  }

  // =========================
  // More Returns Than Arguments Tests
  // =========================

  test("applyMulti: 3 returns with 2 args - all args used at least once succeeds") {
    case class Data(value: Int)
    val a = Data(1)
    val b = Data(2)

    val result = RestrictedSelectable.LinearFn.applyMulti((a, b))(refs =>
      (refs._1, refs._2, refs._1)  // refs._1 used in positions 0 and 2
    )

    assertEquals(result._1.value, 1)
    assertEquals(result._2.value, 2)
    assertEquals(result._3.value, 1)
  }

  test("applyMulti: 4 returns with 2 args - all args used multiple times succeeds") {
    case class Data(value: Int)
    val a = Data(1)
    val b = Data(2)

    val result = RestrictedSelectable.LinearFn.applyMulti((a, b))(refs =>
      (refs._1, refs._2, refs._1, refs._2)
    )

    assertEquals(result._1.value, 1)
    assertEquals(result._2.value, 2)
    assertEquals(result._3.value, 1)
    assertEquals(result._4.value, 2)
  }

  test("applyMulti: 5 returns with 3 args - complex distribution succeeds") {
    case class Data(value: Int)
    val a = Data(1)
    val b = Data(2)
    val c = Data(3)

    val result = RestrictedSelectable.LinearFn.applyMulti((a, b, c))(refs =>
      (refs._1, refs._2, refs._3, refs._1, refs._2)
    )

    assertEquals(result._1.value, 1)
    assertEquals(result._2.value, 2)
    assertEquals(result._3.value, 3)
    assertEquals(result._4.value, 1)
    assertEquals(result._5.value, 2)
  }

  test("applyMulti: 3 returns with 2 args - duplicate in single return fails") {
    val obtained = compileErrors("""
      case class Combined(a: Int, b: Int)
      case class Data(value: Int)
      val a = Data(1)
      val b = Data(2)

      RestrictedSelectable.LinearFn.applyMulti((a, b))(refs =>
        // Manually create a return value that uses both args
        val combined = RestrictedSelectable.Restricted.LinearRef[Combined, (0, 1), EmptyTuple](() => Combined(1, 2))
        (combined, refs._1, refs._2)  // combined uses both 0 and 1
      )
    """)
    assert(obtained.contains(TestUtils.restrictedTypesMsg), s"obtained: $obtained")
  }

  test("applyMulti: 3 returns with 2 args - missing arg fails") {
    val obtained = compileErrors("""
      case class Data(value: Int)
      val a = Data(1)
      val b = Data(2)

      RestrictedSelectable.LinearFn.applyMulti((a, b))(refs =>
        (refs._1, refs._1, refs._1)  // Missing refs._2
      )
    """)
    assert(obtained.contains(TestUtils.linearMsg), s"obtained: $obtained")
  }

  // =========================
  // applyConsumedMulti Tests
  // =========================

  test("applyConsumedMulti: with unconsumed values fails") {
    val obtained = compileErrors("""
      case class Data(value: Int)
      val a = Data(1)
      val b = Data(2)

      RestrictedSelectable.LinearFn.applyConsumedMulti((a, b))(refs =>
        // refs._1 and refs._2 are unconsumed (C = EmptyTuple)
        (refs._1, refs._2, refs._1)
      )
    """)
    assert(obtained.contains(TestUtils.consumptionExactlyOneMsg), s"obtained: $obtained")
  }

  test("applyConsumedMulti: verifies consumption state requirement") {
    // Test that applyConsumedMulti enforces consumption state
    // We'll just verify the compile error message is specific to consumption
    val obtained = compileErrors("""
      case class Box(value: Int)
      val a = Box(1)
      val b = Box(2)

      RestrictedSelectable.LinearFn.applyConsumedMulti((a, b))(refs =>
        (refs._1, refs._2, refs._1, refs._2)
      )
    """)
    assert(obtained.contains(TestUtils.consumptionExactlyOneMsg), s"obtained: $obtained")
  }

  // =========================
  // Edge Cases
  // =========================

  test("applyMulti: 1 arg with 3 returns - all same arg succeeds") {
    case class Data(value: Int)
    val a = Data(42)

    val result = RestrictedSelectable.LinearFn.applyMulti(Tuple1(a))(refs =>
      (refs._1, refs._1, refs._1)
    )

    assertEquals(result._1.value, 42)
    assertEquals(result._2.value, 42)
    assertEquals(result._3.value, 42)
  }

  test("applyMulti: complex nested usage") {
    case class Point(x: Int, y: Int)
    val p1 = Point(1, 2)
    val p2 = Point(3, 4)
    val p3 = Point(5, 6)

    val result = RestrictedSelectable.LinearFn.applyMulti((p1, p2, p3))(refs =>
      // Use each point at least once, distribute across 5 returns
      (refs._1, refs._2, refs._3, refs._1, refs._2)
    )

    assertEquals(result._1.x, 1)
    assertEquals(result._2.x, 3)
    assertEquals(result._3.x, 5)
    assertEquals(result._4.x, 1)
    assertEquals(result._5.x, 3)
  }

  // =========================
  // Verification that regular apply still works
  // =========================

  test("regular apply: still enforces equal args and returns") {
    val obtained = compileErrors("""
      case class Data(value: Int)
      val a = Data(1)
      val b = Data(2)

      RestrictedSelectable.LinearFn.apply((a, b))(refs =>
        (refs._1, refs._2, refs._1)  // 3 returns for 2 args
      )
    """)
    assert(obtained.contains(TestUtils.argsMsg), s"obtained: $obtained")
  }
