package test

import munit.FunSuite
import linearfn.{RestrictedSelectable, Multiplicity, ops, consumed, unconsumed, repeatable}
import scala.annotation.experimental

/**
 * Comprehensive tests for @ops annotation with @consumed, @unconsumed, and @repeatable.
 *
 * Tests the interaction between:
 * - Multiplicities: Linear (exactly once), Affine (at most once), Relevant (at least once)
 * - Annotations: @consumed, @unconsumed, @repeatable
 * - ForAll-Affine + ForAll-Relevant semantics for plain tuples
 */
@experimental
class DefaultConnectiveTest extends FunSuite:
  import OpsExampleOps.*
  // ============================================================================
  // Tests for ForAll-Affine + ForAll-Relevant semantics
  // ============================================================================

  test("General affine: all arguments can be used") {
    val a = OpsExample("a")
    val b = OpsExample("b")

    val result = RestrictedSelectable.RestrictedFn.apply(Multiplicity.Affine)((a, b))(refs =>
      (refs._1, refs._2) // Both args used exactly once
    )

    assertEquals(result._1.name, "a")
    assertEquals(result._2.name, "b")
  }
  test("General affine: fewer arguments but unified ok") {
    val a = OpsExample("a")
    val b = OpsExample("b")

    val result = RestrictedSelectable.RestrictedFn.apply(Multiplicity.Affine)((a, b))(refs =>
      Tuple1(refs._1.singleRestrictedProductArg(refs._2)) // Both refs used exactly once
    )

    assertEquals(result._1.name, "a & b")
  }
  test("General affine: more arguments but unified ok") {
    val a = OpsExample("a")
    val b = OpsExample("b")

    val result = RestrictedSelectable.RestrictedFn.apply(Multiplicity.Affine)((a, b))(refs =>
      import scala.language.implicitConversions
      import RestrictedSelectable.Restricted.given
      val capturedA: RestrictedSelectable.Restricted[OpsExample, EmptyTuple, EmptyTuple] = a.consume()
      (refs._1.consume(), refs._2.consume(), capturedA) // Both refs used exactly once
    )

    assertEquals(result._1.name, "a")
    assertEquals(result._2.name, "b")
    assertEquals(result._3.name, "a")
  }

  test("General affine: NEGATIVE - argument used twice fails") {
    val obtained = compileErrors(
      """
      import OpsExampleOps.*
      import linearfn.{RestrictedSelectable, Multiplicity}

      val a = OpsExample("a")
      val b = OpsExample("b")

      RestrictedSelectable.RestrictedFn.apply(Multiplicity.Affine)((a, b))(refs =>
        (refs._1.consume(), refs._2.singleRestrictedProductArg(refs._2).consume())  // Error: refs._2 used twice
      )
    """)

    assert(
      obtained.contains(TestUtils.compositionForEachFailed),
      s"Expected ForAll-Affine error but got: $obtained"
    )
  }

  test("General linear: all arguments can be used") {
    val a = OpsExample("a")
    val b = OpsExample("b")

    val result = RestrictedSelectable.RestrictedFn.apply(Multiplicity.Linear)((a, b))(refs =>
      (refs._1.consume(), refs._2.consume()) // Both args used exactly once
    )

    assertEquals(result._1.name, "a")
    assertEquals(result._2.name, "b")
  }
  test("General linear: fewer arguments but unified ok") {
    val a = OpsExample("a")
    val b = OpsExample("b")

    val result = RestrictedSelectable.RestrictedFn.apply(Multiplicity.Linear)((a, b))(refs =>
      Tuple1(refs._1.singleRestrictedProductArg(refs._2).consume()) // Both refs used exactly once
    )

    assertEquals(result._1.name, "a & b")
  }
  test("General linear: more arguments but unified ok") {
    val a = OpsExample("a")
    val b = OpsExample("b")

    val result = RestrictedSelectable.RestrictedFn.apply(Multiplicity.Linear)((a, b))(refs =>
      import scala.language.implicitConversions
      import RestrictedSelectable.Restricted.given
      val capturedA: RestrictedSelectable.Restricted[OpsExample, EmptyTuple, EmptyTuple] = a.consume()
      (refs._1.consume(), refs._2.consume(), capturedA) // Both refs used exactly once
    )

    assertEquals(result._1.name, "a")
    assertEquals(result._2.name, "b")
    assertEquals(result._3.name, "a")
  }
  test("General linear: NEGATIVE - missing argument fails") {
    val obtained = compileErrors(
      """
      import OpsExampleOps.*
      import linearfn.{RestrictedSelectable, Multiplicity}

      val a = OpsExample("a")
      val b = OpsExample("b")

      RestrictedSelectable.RestrictedFn.apply(Multiplicity.Linear)((a, b))(refs =>
        Tuple1(refs._1)  // Error: refs._2 not used
      )
    """)

    assert(
      obtained.contains(TestUtils.multiplicityConstraintFailed),
      s"Expected ForAll-Relevant error but got: $obtained"
    )
  }
  test("General linear: NEGATIVE - argument used twice fails") {
    val obtained = compileErrors(
      """
      import OpsExampleOps.*
      import linearfn.{RestrictedSelectable, Multiplicity}

      val a = OpsExample("a")
      val b = OpsExample("b")

      RestrictedSelectable.RestrictedFn.apply(Multiplicity.Linear)((a, b))(refs =>
        (refs._1, refs._2, refs._1)  // Error: refs._1 used twice
      )
    """)

    assert(
      obtained.contains(TestUtils.multiplicityConstraintFailed),
      s"Expected ForAll-Affine error but got: $obtained"
    )
  }

  test("General relevant: all arguments can be used") {
    val a = OpsExample("a")
    val b = OpsExample("b")

    val result = RestrictedSelectable.RestrictedFn.apply(Multiplicity.Relevant)((a, b))(refs =>
      (refs._1.consume(), refs._2.consume()) // Both args used exactly once
    )

    assertEquals(result._1.name, "a")
    assertEquals(result._2.name, "b")
  }
  test("General relevant: fewer arguments but unified ok") {
    val a = OpsExample("a")
    val b = OpsExample("b")

    val result = RestrictedSelectable.RestrictedFn.apply(Multiplicity.Relevant)((a, b))(refs =>
      Tuple1(refs._1.singleRestrictedProductArg(refs._2).consume()) // Both refs used exactly once
    )

    assertEquals(result._1.name, "a & b")
  }
  test("General relevant: more arguments but unified ok") {
    val a = OpsExample("a")
    val b = OpsExample("b")

    val result = RestrictedSelectable.RestrictedFn.apply(Multiplicity.Relevant)((a, b))(refs =>
      import scala.language.implicitConversions
      import RestrictedSelectable.Restricted.given
      val capturedA: RestrictedSelectable.Restricted[OpsExample, EmptyTuple, EmptyTuple] = a
      (refs._1.consume(), refs._2.consume(), capturedA) // Both refs used exactly once
    )

    assertEquals(result._1.name, "a")
    assertEquals(result._2.name, "b")
    assertEquals(result._3.name, "a")
  }
  test("General relevant: NEGATIVE - missing argument fails") {
    val obtained = compileErrors(
      """
      import OpsExampleOps.*
      import linearfn.{RestrictedSelectable, Multiplicity}

      val a = OpsExample("a")
      val b = OpsExample("b")

      RestrictedSelectable.RestrictedFn.apply(Multiplicity.Relevant)((a, b))(refs =>
        Tuple1(refs._1)  // Error: refs._2 not used
      )
    """)

    assert(
      obtained.contains(TestUtils.multiplicityConstraintFailed),
      s"Expected ForAll-Relevant error but got: $obtained"
    )
  }
  test("General relevant: arguments can be used twice") {
    val a = OpsExample("a")
    val b = OpsExample("b")

    val result = RestrictedSelectable.RestrictedFn.apply(Multiplicity.Relevant)((a, b))(refs =>
      (refs._1.singleRestrictedProductArg(refs._2).consume(), refs._2.consume())
    )

    assertEquals(result._1.name, "a & b")
    assertEquals(result._2.name, "b")
  }

