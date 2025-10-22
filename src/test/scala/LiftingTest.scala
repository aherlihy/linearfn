package linearfn

import munit.FunSuite
import scala.annotation.experimental
import OpsExampleOps.*

/**
 * Tests for automatic lifting of nested types: T[Restricted[A, D]] => Restricted[T[A], D]
 *
 * This prevents containers of Restricted values from evading linearity checks.
 * Without lifting, `List[Restricted[A, D]]` would be a plain List that could be
 * returned multiple times, violating linearity. With lifting, it's automatically
 * converted to `Restricted[List[A], D]` which is properly tracked.
 */
@experimental
class LiftingTest extends FunSuite:
  import TestUtils.*

  test("List[Restricted[A, D]] is automatically lifted in 2-tuple") {
    val ex1 = OpsExample("Alice", "30")
    val ex2 = OpsExample("Bob", "25")

    val result = RestrictedSelectable.LinearFn.apply((ex1, ex2))(refs =>
      // Create a list containing one Restricted element
      // The tuple conversion should automatically lift it
      (List(refs._1), refs._2)
    )

    assertEquals(result._1, List(ex1))
    assertEquals(result._2, ex2)
  }

  test("Option[Restricted[A, D]] is automatically lifted in 1-tuple") {
    val ex1 = OpsExample("Alice", "30")

    val result = RestrictedSelectable.LinearFn.apply(Tuple1(ex1))(refs =>
      // The tuple conversion should automatically lift the Option
      Tuple1(Option(refs._1))
    )

    assertEquals(result._1, Some(ex1))
  }

  test("Vector[Restricted[A, D]] is automatically lifted in 1-tuple") {
    val ex1 = OpsExample("Alice", "30")

    val result = RestrictedSelectable.LinearFn.apply(Tuple1(ex1))(refs =>
      // The tuple conversion should automatically lift the Vector
      Tuple1(Vector(refs._1))
    )

    assertEquals(result._1, Vector(ex1))
  }

  test("linearity violation: same ref in two Lists") {
    val obtained = compileErrors("""
      val ex1 = OpsExample("Alice", "30")
      val ex2 = OpsExample("Bob", "25")
      RestrictedSelectable.LinearFn.apply((ex1, ex2))(refs =>
        (List(refs._1), List(refs._1))
      )
    """)
    // This should fail because refs._1 is used in two different lists
    // Both lists would have dependency Tuple1[0], violating linearity
    // Correct number of args (2 in, 2 out), but refs._1 used twice
    assert(obtained.contains(linearMsg), s"obtained: $obtained")
  }

  test("linearity violation: returning List and the element inside it") {
    val obtained = compileErrors("""
      val ex1 = OpsExample("Alice", "30")
      val ex2 = OpsExample("Bob", "25")
      val ex3 = OpsExample("Charlie", "35")
      RestrictedSelectable.LinearFn.apply((ex1, ex2, ex3))(refs =>
        (List(refs._1), refs._1, refs._2)
      )
    """)
    // This should fail because we're returning both a List containing refs._1
    // and refs._1 itself - that's using refs._1 twice
    // Correct number of args (3 in, 3 out), but refs._1 used twice
    assert(obtained.contains(linearMsg), s"obtained: $obtained")
  }

  test("linearity violation: same ref in Option and List") {
    val obtained = compileErrors("""
      val ex1 = OpsExample("Alice", "30")
      val ex2 = OpsExample("Bob", "25")
      RestrictedSelectable.LinearFn.apply((ex1, ex2))(refs =>
        (Option(refs._1), List(refs._1))
      )
    """)
    // This should fail because refs._1 appears in both Option and List
    // Correct number of args (2 in, 2 out), but refs._1 used twice
    assert(obtained.contains(linearMsg), s"obtained: $obtained")
  }

  test("wrong number of return arguments with nested types") {
    val obtained = compileErrors("""
      val ex1 = OpsExample("Alice", "30")
      RestrictedSelectable.LinearFn.apply(Tuple1(ex1))(refs =>
        (List(refs._1), List(refs._1))
      )
    """)
    // This tests that we still catch wrong number of arguments
    // 1 arg in, but returning 2 values (both Lists)
    assert(obtained.contains("Number of actual arguments must match"), s"obtained: $obtained")
  }

  test("linearity OK: different refs in different containers") {
    val ex1 = OpsExample("Alice", "30")
    val ex2 = OpsExample("Bob", "25")

    // This is fine - refs._1 in first container, refs._2 in second
    val result = RestrictedSelectable.LinearFn.apply((ex1, ex2))(refs =>
      (List(refs._1), Option(refs._2))
    )

    assertEquals(result._1, List(ex1))
    assertEquals(result._2, Some(ex2))
  }
