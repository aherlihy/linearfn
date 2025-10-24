package test

import munit.FunSuite
import linearfn.{RestrictedSelectable, ops, restrictedFn, unrestricted}
import scala.annotation.experimental

/**
 * Tests for @restrictedFn parameter-level annotation.
 *
 * @restrictedFn allows precise control over function parameters where the function's
 * return type should be wrapped in Restricted, but not the function itself.
 */

// Test case for higher-order functions with @restrictedFn
@ops
case class TestQuery[A](data: List[A]):
  def flatMap[B](@restrictedFn f: A => TestQuery[B]): TestQuery[B] =
    TestQuery(data.flatMap(a => f(a).data))

  def map[B](@unrestricted f: A => B): TestQuery[B] =
    TestQuery(data.map(f))

  def filter(@unrestricted p: A => Boolean): TestQuery[A] =
    TestQuery(data.filter(p))

  // Regular function (default behavior - entire function wrapped)
  def combine[B](f: A => TestQuery[B]): TestQuery[B] =
    TestQuery(data.flatMap(a => f(a).data))

@experimental
class RestrictedAnnotationTest extends FunSuite:
  import TestUtils.*

  test("TestQuery.map with @unrestricted - doesn't track function") {
    import TestQueryOps.*

    val q = TestQuery(List(1, 2, 3))

    val result = RestrictedSelectable.LinearFn.apply(Tuple1(q))(refs =>
      Tuple1(refs._1.map((x: Int) => x * 2))
    )

    assertEquals(result._1.data, List(2, 4, 6))
  }

   test("TestQuery.flatMap with @restrictedFn - tracks inner TestQuery[B] dependencies") {
     import TestQueryOps.*

     val q1 = TestQuery(List(1, 2, 3))
     val q2 = TestQuery(List(10, 20))

     val result = RestrictedSelectable.LinearFn.apply((q1, q2))(refs =>
       (refs._1.flatMap(x => refs._2), refs._1)
     )

     assertEquals(result._1.data, List(10, 20, 10, 20, 10, 20))
   }

  test("TestQuery.filter with @unrestricted - doesn't track predicate") {
    import TestQueryOps.*

    val q = TestQuery(List(1, 2, 3, 4, 5))

    val result = RestrictedSelectable.LinearFn.apply(Tuple1(q))(refs =>
      val filtered = refs._1.filter((x: Int) => x > 2)
      Tuple1(filtered)
    )

    assertEquals(result._1.data, List(3, 4, 5))
  }

  test("Method type parameters are preserved") {
    import TestQueryOps.*

    // This test verifies that method type parameters like [B] in flatMap[B]
    // are correctly preserved in the generated extension methods
    val q = TestQuery(List(1, 2, 3))

    val result = RestrictedSelectable.LinearFn.apply(Tuple1(q))(refs =>
      // [String] should be preserved as a type parameter
      val stringQuery = refs._1.map[String]((x: Int) => x.toString)
      Tuple1(stringQuery)
    )

    assertEquals(result._1.data, List("1", "2", "3"))
  }

  test("NEGATIVE: flatMap requires linear use of inner query") {
    val obtained = compileErrors("""
       import TestQueryOps.*
       import linearfn.RestrictedSelectable

       val q1 = TestQuery(List(1, 2))
       val q2 = TestQuery(List(10, 20))

       RestrictedSelectable.LinearFn.apply((q1, q2))(refs =>
         val combined = refs._2.flatMap((x: Int) => refs._2)
         // Error: refs._2 used twice (once in flatMap body, once in return)
         (combined, refs._2)
       )
     """)

    assert(obtained.contains(affineMsg), s"Expected linearity error but got: $obtained")
  }

  test("@restrictedFn runtime correctness - function actually receives unwrapped value") {
    import TestQueryOps.*

    val q1 = TestQuery(List(1, 2, 3))
    val q2 = TestQuery(List(100))

    // Test that the function parameter receives an Int (not Restricted[Int, _, _])
    // and can return a Restricted query that gets properly unwrapped
    val result = RestrictedSelectable.LinearFn.apply((q1, q2))(refs =>
      val transformed = refs._1.flatMap { (x: Int) =>
        // x should be a plain Int
        // we return refs._2 which should be properly tracked
        refs._2
      }
      (transformed, refs._1)
    )

    // Each element of q1 (1, 2, 3) should be mapped to q2's contents (100)
    assertEquals(result._1.data, List(100, 100, 100))
  }

  test("@restrictedFn runtime correctness - nested function calls") {
    import TestQueryOps.*

    val q1 = TestQuery(List(1, 2))
    val q2 = TestQuery(List(10, 20))
    val q3 = TestQuery(List(100, 200, 300))

    val result = RestrictedSelectable.LinearFn.apply((q1, q2, q3))(refs =>
      // Chain multiple flatMaps
      val step1 = refs._1.flatMap { x =>
        refs._2  // For each element in q1, return q2
      }
      val step2 = step1.flatMap { y =>
        refs._3  // For each element in step1, return q3
      }
      (step2, refs._1, refs._2)
    )

    // q1 has 2 elements, each mapped to q2 (2 elements) = 4 elements
    // Those 4 elements each mapped to q3 (3 elements) = 12 elements
    assertEquals(result._1.data.length, 12)
    assertEquals(result._1.data.forall(x => List(100, 200, 300).contains(x)), true, "All elements should be from q3")
  }

  test("@restrictedFn runtime correctness - mixed tracked and unrestricted") {
    import TestQueryOps.*

    val q = TestQuery(List(1, 2, 3))

    val result = RestrictedSelectable.LinearFn.apply(Tuple1(q))(refs =>
      // First use @unrestricted map to transform values
      val doubled = refs._1.map((x: Int) => x * 2)
      // Then use filter (also @unrestricted)
      val filtered = doubled.filter((x: Int) => x > 2)
      Tuple1(filtered)
    )

    assertEquals(result._1.data, List(4, 6))
  }

  test("Regular function parameter (no annotation) - tracks entire function via implicit") {
    import TestQueryOps.*
    import RestrictedSelectable.Restricted.given

    val q1 = TestQuery(List(1, 2, 3))

    // combine takes a regular function parameter (wrapped in Restricted via implicit conversion)
    // The function itself is not tracked, it's just a plain function wrapped via implicit
    val result = RestrictedSelectable.LinearFn.apply(Tuple1(q1))(refs =>
      // Plain function is implicitly converted to Restricted[A => TestQuery[B], EmptyTuple, EmptyTuple]
      val transformed = refs._1.combine((x: Int) => TestQuery(List(x * 2)))
      Tuple1(transformed)
    )

    assertEquals(result._1.data, List(2, 4, 6))
  }

  test("Regular function vs @restrictedFn vs @unrestricted - comparison") {
    import TestQueryOps.*

    val q1 = TestQuery(List(1, 2))
    val q2 = TestQuery(List(100))

    // @restrictedFn: only tracks return type, so we can return refs._2
    val result1 = RestrictedSelectable.LinearFn.apply((q1, q2))(refs =>
      (refs._1.flatMap((x: Int) => refs._2), refs._1)
    )
    assertEquals(result1._1.data, List(100, 100))

    // @unrestricted: doesn't track anything
    val result2 = RestrictedSelectable.LinearFn.apply(Tuple1(q1))(refs =>
      Tuple1(refs._1.map((x: Int) => x * 10))
    )
    assertEquals(result2._1.data, List(10, 20))

    // Regular (no annotation): tracks entire function via implicit conversion
    // Can't reference refs inside because function is implicitly converted (not tracked dynamically)
    val result3 = RestrictedSelectable.LinearFn.apply(Tuple1(q1))(refs =>
      Tuple1(refs._1.combine((x: Int) => TestQuery(List(x + 1000))))
    )
    assertEquals(result3._1.data, List(1001, 1002))
  }

  // Note: Negative tests for @restrictedFn are validated at build time by OpsExtensionGenerator
  // and produce warnings during source generation, not compile-time errors that can be caught
  // by compileErrors. The following would fail during sbt compilation with appropriate errors:
  //
  // INVALID: @restrictedFn on non-function parameter
  // @ops
  // case class InvalidTest1[A](opt: Option[A]):
  //   def bad(@restrictedFn fallback: Option[A]): A = opt.getOrElse(fallback.get)
  // Error: "@restrictedFn can only be used on function parameters, not on Option[A]"
  //
  // INVALID: @restrictedFn on multi-parameter function
  // @ops
  // case class InvalidTest2[A](items: List[A]):
  //   def bad(@restrictedFn f: (A, A) => A): List[A] = items.map(x => f(x, x))
  // Error: "@restrictedFn can only be used on single-parameter functions (A => B), not on (A, A) => A which has 2 parameters"
