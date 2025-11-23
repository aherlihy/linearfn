package test

import munit.FunSuite
import linearfn.{RestrictedSelectable, Multiplicity, ops}
import scala.annotation.experimental
import linearfn.RestrictedSelectable.{given, *}


/**
 * Comprehensive tests for custom connectives
 *
 */
@experimental
class DefaultConnectiveTest extends FunSuite:
  import OpsExampleOps.*

  test("General affine: all arguments returned in tuple") {
    val a = OpsExample("a")
    val b = OpsExample("b")

    val result = RestrictedFn.apply(Multiplicity.Affine)((a, b))(refs =>
      ForAllAffineConnective((refs._1, refs._2)) // Both args used exactly once
    )

    assertEquals(result._1.name, "a")
    assertEquals(result._2.name, "b")
  }
  test("General affine: all arguments returned in tuple length < args") {
    val a = OpsExample("a")
    val b = OpsExample("b")

    val result = RestrictedFn.apply(Multiplicity.Affine)((a, b))(refs =>
      ForAllAffineConnective(Tuple1(refs._1.singleRestrictedProductArg(refs._2))) // Both refs used exactly once
    )

    assertEquals(result._1.name, "a & b")
  }
  test("General affine: all arguments returned in tuple length > args (conversion for extra arg)") {
    val a = OpsExample("a")
    val b = OpsExample("b")

    val result = RestrictedFn.apply(Multiplicity.Affine)((a, b))(refs =>
      import scala.language.implicitConversions
      import Restricted.given
      val capturedA: Restricted[OpsExample, EmptyTuple] = a
      ForAllAffineConnective((refs._1, refs._2, capturedA)) // Both refs used exactly once
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

      RestrictedFn.apply(Multiplicity.Affine)((a, b))(refs =>
        ForAllAffineConnective((refs._1, refs._2.singleRestrictedProductArg(refs._2)))  // Error: refs._2 used twice
      )
    """)

    assert(
      obtained.contains(TestUtils.forAll) && obtained.contains(TestUtils.affine),
      s"Expected ForAll-Affine error but got: $obtained"
    )
  }

  test("General linear: all arguments returned in tuple") {
    val a = OpsExample("a")
    val b = OpsExample("b")

    val result = RestrictedFn.apply(Multiplicity.Linear)((a, b))(refs =>
      ForAllLinearConnective((refs._1, refs._2)) // Both args used exactly once
    )

    assertEquals(result._1.name, "a")
    assertEquals(result._2.name, "b")
  }
  test("General linear: all arguments returned in tuple length < args") {
    val a = OpsExample("a")
    val b = OpsExample("b")

    val result = RestrictedFn.apply(Multiplicity.Linear)((a, b))(refs =>
      ForAllLinearConnective(Tuple1(refs._1.singleRestrictedProductArg(refs._2))) // Both refs used exactly once
    )

    assertEquals(result._1.name, "a & b")
  }
  test("General linear: all arguments returned in tuple length > args (conversion for extra arg)") {
    val a = OpsExample("a")
    val b = OpsExample("b")

    val result = RestrictedFn.apply(Multiplicity.Linear)((a, b))(refs =>
      import scala.language.implicitConversions
      import Restricted.given
      val capturedA: Restricted[OpsExample, EmptyTuple] = a
      ForAllLinearConnective((refs._1, refs._2, capturedA)) // Both refs used exactly once
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

      RestrictedFn.apply(Multiplicity.Linear)((a, b))(refs =>test
        ForAllLinearConnective(Tuple1(refs._1))  // Error: refs._2 not used
      )
    """)

    assert(
      obtained.contains(TestUtils.noGivenInstance) && obtained.contains(TestUtils.linear),
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

      RestrictedFn.apply(Multiplicity.Linear)((a, b))(refs =>
        ForAllLinearConnective(refs._1, refs._2, refs._1))  // Error: refs._1 used twice
      )
    """)

    assert(
      obtained.contains(TestUtils.forAll) && obtained.contains(TestUtils.linear),
      s"Expected ForEach-Affine error but got: $obtained"
    )
  }

  test("General relevant: all arguments returned in tuple") {
    val a = OpsExample("a")
    val b = OpsExample("b")

    val result = RestrictedFn.apply(Multiplicity.Relevant)((a, b))(refs =>
      ForAllRelevantConnective((refs._1, refs._2)) // Both args used exactly once
    )

    assertEquals(result._1.name, "a")
    assertEquals(result._2.name, "b")
  }
  test("General relevant: all arguments returned in tuple length < args") {
    val a = OpsExample("a")
    val b = OpsExample("b")

    val result = RestrictedFn.apply(Multiplicity.Relevant)((a, b))(refs =>
      ForAllRelevantConnective(Tuple1(refs._1.singleRestrictedProductArg(refs._2))) // Both refs used exactly once
    )

    assertEquals(result._1.name, "a & b")
  }
  test("General relevant: all arguments returned in tuple length > args (conversion for extra arg)") {
    val a = OpsExample("a")
    val b = OpsExample("b")

    val result = RestrictedFn.apply(Multiplicity.Relevant)((a, b))(refs =>
      import scala.language.implicitConversions
      import Restricted.given
      val capturedA: Restricted[OpsExample, EmptyTuple] = a
      ForAllRelevantConnective((refs._1, refs._2, capturedA)) // Both refs used exactly once
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

      RestrictedFn.apply(Multiplicity.Relevant)((a, b))(refs =>
        ForAllRelevantConnective(Tuple1(refs._1))  // Error: refs._2 not used
      )
    """)

    assert(
      obtained.contains(TestUtils.forAll) && obtained.contains(TestUtils.relevant),
      s"Expected ForAll-Relevant error but got: $obtained"
    )
  }
  test("General relevant: all arguments must be used at least once") {
    val a = OpsExample("a")
    val b = OpsExample("b")

    val result = RestrictedFn.apply(Multiplicity.Relevant)((a, b))(refs =>
      ForAllRelevantConnective((refs._1, refs._2))
    )

    assertEquals(result._1.name, "a")
    assertEquals(result._2.name, "b")
  }

