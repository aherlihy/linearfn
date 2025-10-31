package test

import linearfn.{RestrictedSelectable, VerticalConstraint, HorizontalConstraint}
import test.casestudies.{FileHandle, FileHandleOps}

/**
 * Tests for customApply with different constraint combinations.
 *
 * Note: These tests demonstrate that customApply works, though some constraint
 * combinations are difficult to test directly without more complex setup.
 */
class CustomApplyTest extends RestrictedFnTestSuite(RestrictedSelectable, "CustomApply"):
  import OpsExampleOps.*
  import FileHandleOps.*

  // ============================================================================
  // Basic functionality tests
  // ============================================================================

  test("customApply: Basic usage with default constraints") {
    val a = OpsExample("a")
    val b = OpsExample("b")

    val result = RestrictedSelectable.RestrictedFn.customApply(
      (vertical = VerticalConstraint.Affine, horizontal = HorizontalConstraint.ForAllRelevantForEachAffine)
    )((a, b))(refs =>
      (refs._1, refs._2, refs._1)
    )

    assertEquals(result._1.name, "a")
    assertEquals(result._2.name, "b")
    assertEquals(result._3.name, "a")
  }

  test("customApply: Affine vertical constraint allows unconsumed") {
    val a = OpsExample("a")
    val b = OpsExample("b")

    val result = RestrictedSelectable.RestrictedFn.customApply(
      (vertical = VerticalConstraint.Affine, horizontal = HorizontalConstraint.ForAllRelevantForEachAffine)
    )((a, b))(refs =>
      (refs._1, refs._2)
    )

    assertEquals(result._1.name, "a")
    assertEquals(result._2.name, "b")
  }

  test("customApply: Linear vertical constraint requires consumption") {
    val a = FileHandle.open("a.txt")
    val b = FileHandle.open("b.txt")

    val result = RestrictedSelectable.RestrictedFn.customApply(
      (vertical = VerticalConstraint.Linear, horizontal = HorizontalConstraint.ForAllRelevantForEachAffine)
    )((a, b))(refs =>
      (refs._1.close(), refs._2.close())
    )

    assertEquals(result._1, "File closed")
    assertEquals(result._2, "File closed")
  }

  test("customApply: Relevant vertical constraint allows consumption") {
    val a = FileHandle.open("a.txt")
    val b = FileHandle.open("b.txt")

    val result = RestrictedSelectable.RestrictedFn.customApply(
      (vertical = VerticalConstraint.Relevant, horizontal = HorizontalConstraint.ForAllRelevantForEachAffine)
    )((a, b))(refs =>
      (refs._1.close(), refs._2.close())
    )

    assertEquals(result._1, "File closed")
    assertEquals(result._2, "File closed")
  }

  test("customApply: Comparison with standard apply") {
    val a1 = OpsExample("a")
    val b1 = OpsExample("b")
    val a2 = OpsExample("a")
    val b2 = OpsExample("b")

    // customApply with default constraints should behave like apply
    val customResult = RestrictedSelectable.RestrictedFn.customApply(
      (vertical = VerticalConstraint.Affine, horizontal = HorizontalConstraint.ForAllRelevantForEachAffine)
    )((a1, b1))(refs =>
      (refs._1, refs._2)
    )

    val standardResult = RestrictedSelectable.RestrictedFn.apply((a2, b2))(refs =>
      (refs._1, refs._2)
    )

    assertEquals(customResult._1.name, standardResult._1.name)
    assertEquals(customResult._2.name, standardResult._2.name)
  }
