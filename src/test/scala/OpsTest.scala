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
class OpsTest extends FunSuite:
  import MArrayOps.*
  import OpsExampleOps.*

  // ============================================================================
  // Tests for @consumed annotation with different multiplicities
  // ============================================================================

  test("@consumed with Affine/Linear/Relevant: allows returning consumed value") {
    val arr = MArray[Int](Array(1, 2, 3))

    val result1 = RestrictedSelectable.RestrictedFn.apply(Multiplicity.Affine)(Tuple1(arr))(refs =>
      val updated = refs._1.write(0, 10)
      val frozen = updated.freeze()  // @consumed
      Tuple1(frozen)
    )
    assertEquals(result1._1.toList, List(10, 2, 3))

    val result2 = RestrictedSelectable.RestrictedFn.apply(Multiplicity.Linear)(Tuple1(arr))(refs =>
      val updated = refs._1.write(0, 10)
      val frozen = updated.freeze()  // @consumed
      Tuple1(frozen)
    )
    assertEquals(result2._1.toList, List(10, 2, 3))

    val result3 = RestrictedSelectable.RestrictedFn.apply(Multiplicity.Relevant)(Tuple1(arr))(refs =>
      val updated = refs._1.write(0, 10)
      val frozen = updated.freeze()  // @consumed
      Tuple1(frozen)
    )
    assertEquals(result3._1.toList, List(10, 2, 3))
  }

  test("@consumed with Affine: allows returning unconsumed value") {
    val arr = MArray[Int](Array(1, 2, 3))

    val result = RestrictedSelectable.RestrictedFn.apply(Multiplicity.Affine)(Tuple1(arr))(refs =>
      val updated = refs._1.write(0, 10)
      Tuple1(updated)
    )

    assertEquals(result._1.freeze().toSeq, Array(10, 2, 3).toSeq)
  }

  test("@consumed with Linear: NEGATIVE - fails if not consumed") {
    val obtained = compileErrors("""
      import MArrayOps.*
      import linearfn.{RestrictedSelectable, Multiplicity}

      val arr = MArray[Int](Array(1, 2, 3))

      RestrictedSelectable.RestrictedFn.apply(Multiplicity.Linear)(Tuple1(arr))(refs =>
        val updated = refs._1.write(0, 10)
        Tuple1(updated)  // Error: must consume (call freeze)
      )
    """)

    assert(
      obtained.contains(TestUtils.multiplicityConstraintFailed),
      s"Expected consumption requirement error but got: $obtained"
    )
  }
  test("@consumed with Relevant: NEGATIVE - fails if not consumed") {
    val obtained = compileErrors(
      """
      import MArrayOps.*
      import linearfn.{RestrictedSelectable, Multiplicity}

      val arr = MArray[Int](Array(1, 2, 3))

      RestrictedSelectable.RestrictedFn.apply(Multiplicity.Relevant)(Tuple1(arr))(refs =>
        val updated = refs._1.write(0, 10)
        Tuple1(updated)  // Error: must consume (call freeze)
      )
    """)

    assert(
      obtained.contains(TestUtils.multiplicityConstraintFailed),
      s"Expected consumption requirement error but got: $obtained"
    )
  }

  test("@consumed with Linear/Affine/Relevant: NEGATIVE - cannot call methods after consumed") {
    val obtained1 = compileErrors(
      """
      import MArrayOps.*
      import linearfn.{RestrictedSelectable, Multiplicity}

      val arr = MArray[Int](Array(1, 2, 3))

      RestrictedSelectable.RestrictedFn.apply(Multiplicity.Linear)(Tuple1(arr))(refs =>
        val frozen = refs._1.freeze()
        val oops = frozen.write(0, 10)  // Error: write after @consumed freeze
        Tuple1(oops)
      )
    """)

    assert(
      obtained1.contains(TestUtils.missingField),
      s"Expected missing method error but got: $obtained1"
    )
    val obtained2 = compileErrors(
      """
      import MArrayOps.*
      import linearfn.{RestrictedSelectable, Multiplicity}

      val arr = MArray[Int](Array(1, 2, 3))

      RestrictedSelectable.RestrictedFn.apply(Multiplicity.Affine)(Tuple1(arr))(refs =>
        val frozen = refs._1.freeze()
        val oops = frozen.write(0, 10)  // Error: write after @consumed freeze
        Tuple1(oops)
      )
    """)

    assert(
      obtained2.contains(TestUtils.missingField),
      s"Expected missing method error but got: $obtained2"
    )

    val obtained3 = compileErrors(
      """
      import MArrayOps.*
      import linearfn.{RestrictedSelectable, Multiplicity}

      val arr = MArray[Int](Array(1, 2, 3))

      RestrictedSelectable.RestrictedFn.apply(Multiplicity.Relevant)(Tuple1(arr))(refs =>
        val frozen = refs._1.freeze()
        val oops = frozen.write(0, 10)  // Error: write after @consumed freeze
        Tuple1(oops)
      )
    """)

    assert(
      obtained3.contains(TestUtils.missingField),
      s"Expected missing method error but got: $obtained3"
    )
  }

  test("@consumed with Linear/Affine/Relevant: multiple args: each can be consumed independently") {
    val arr1 = MArray[Int](Array(1, 2, 3))
    val arr2 = MArray[Int](Array(4, 5, 6))

    val result1 = RestrictedSelectable.RestrictedFn.apply(Multiplicity.Linear)((arr1, arr2))(refs =>
      val frozen1 = refs._1.freeze()
      val frozen2 = refs._2.freeze()
      (frozen1, frozen2)
    )
    assertEquals(result1._1.toList, List(1, 2, 3))
    assertEquals(result1._2.toList, List(4, 5, 6))

    val result2 = RestrictedSelectable.RestrictedFn.apply(Multiplicity.Affine)((arr1, arr2))(refs =>
      val frozen1 = refs._1.freeze()
      val frozen2 = refs._2.freeze()
      (frozen1, frozen2)
    )
    assertEquals(result2._1.toList, List(1, 2, 3))
    assertEquals(result2._2.toList, List(4, 5, 6))

    val result3 = RestrictedSelectable.RestrictedFn.apply(Multiplicity.Relevant)((arr1, arr2))(refs =>
      val frozen1 = refs._1.freeze()
      val frozen2 = refs._2.freeze()
      (frozen1, frozen2)
    )
    assertEquals(result3._1.toList, List(1, 2, 3))
    assertEquals(result3._2.toList, List(4, 5, 6))
  }

  // ============================================================================
  // Tests for @unconsumed annotation
  // ============================================================================

  test("@unconsumed: can be called on unconsumed value") {
    val arr = MArray[Int](Array(1, 2, 3))

    val result1 = RestrictedSelectable.RestrictedFn.apply(Multiplicity.Affine)(Tuple1(arr))(refs =>
      val size = refs._1.size()  // @unconsumed
      val updated = refs._1.write(0, 10)
      Tuple1(updated)
    )

    assertEquals(result1._1.freeze().toSeq, Array(10, 2, 3).toSeq)

    val result2 = RestrictedSelectable.RestrictedFn.apply(Multiplicity.Linear)(Tuple1(arr))(refs =>
      val size = refs._1.size()  // @unconsumed
      val updated = refs._1.write(0, 10)
      Tuple1(updated.freeze())
    )

    assertEquals(result2._1.toSeq, Array(10, 2, 3).toSeq)

    val result3 = RestrictedSelectable.RestrictedFn.apply(Multiplicity.Relevant)(Tuple1(arr))(refs =>
      val size = refs._1.size()  // @unconsumed
      val updated = refs._1.write(0, 10)
      Tuple1(updated.freeze())
    )

    assertEquals(result3._1.toSeq, Array(10, 2, 3).toSeq)
  }

  test("@unconsumed: can be called after repeatable") {
    val arr = MArray[Int](Array(1, 2, 3))

    val result1 = RestrictedSelectable.RestrictedFn.apply(Multiplicity.Affine)(Tuple1(arr))(refs =>
      val size1 = refs._1.size()  // @unconsumed
      val updated = refs._1.write(0, 10)
      val size2 = updated.size()  // @unconsumed can be called multiple times
      val frozen = updated.freeze()
      Tuple1(frozen)
    )

    assertEquals(result1._1.toList, List(10, 2, 3))
    val result2 = RestrictedSelectable.RestrictedFn.apply(Multiplicity.Linear)(Tuple1(arr))(refs =>
      val size1 = refs._1.size()  // @unconsumed
      val updated = refs._1.write(0, 10)
      val size2 = updated.size()  // @unconsumed can be called multiple times
      val frozen = updated.freeze()
      Tuple1(frozen)
    )

    assertEquals(result2._1.toList, List(10, 2, 3))
    val result3 = RestrictedSelectable.RestrictedFn.apply(Multiplicity.Relevant)(Tuple1(arr))(refs =>
      val size1 = refs._1.size()  // @unconsumed
      val updated = refs._1.write(0, 10)
      val size2 = updated.size()  // @unconsumed can be called multiple times
      val frozen = updated.freeze()
      Tuple1(frozen)
    )

    assertEquals(result3._1.toList, List(10, 2, 3))
  }

  test("@unconsumed: can be called after consuming") {
    val arr = MArray[Int](Array(1, 2, 3))

    val result1 = RestrictedSelectable.RestrictedFn.apply(Multiplicity.Affine)(Tuple1(arr))(refs =>
      val size1 = refs._1.size() // @unconsumed
      val updated = refs._1.seal()
      val size2 = updated.size() // @unconsumed can be called multiple times
      Tuple1(updated)
    )

    assertEquals(result1._1.freeze().toList, List(1, 2, 3))
    val result2 = RestrictedSelectable.RestrictedFn.apply(Multiplicity.Linear)(Tuple1(arr))(refs =>
      val size1 = refs._1.size() // @unconsumed
      val updated = refs._1.seal()
      val size2 = updated.size() // @unconsumed can be called multiple times
      Tuple1(updated)
    )

    assertEquals(result2._1.freeze().toList, List(1, 2, 3))
    val result3 = RestrictedSelectable.RestrictedFn.apply(Multiplicity.Relevant)(Tuple1(arr))(refs =>
      val size1 = refs._1.size() // @unconsumed
      val updated = refs._1.seal()
      val size2 = updated.size() // @unconsumed can be called multiple times
      Tuple1(updated)
    )

    assertEquals(result3._1.freeze().toList, List(1, 2, 3))
  }

  test("@unconsumed: multiple calls preserve state") {
    val arr = MArray[Int](Array(1, 2, 3))

    val result1 = RestrictedSelectable.RestrictedFn.apply(Multiplicity.Linear)(Tuple1(arr))(refs =>
      val size1 = refs._1.size()
      val size2 = refs._1.size()
      val updated = refs._1.write(0, 10)
      Tuple1(updated.freeze())
    )

    assertEquals(result1._1.toSeq, Array(10, 2, 3).toSeq)
    val result2 = RestrictedSelectable.RestrictedFn.apply(Multiplicity.Affine)(Tuple1(arr))(refs =>
      val size1 = refs._1.size()
      val size2 = refs._1.size()
      val updated = refs._1.write(0, 10)
      Tuple1(updated)
    )

    assertEquals(result2._1.freeze().toSeq, Array(10, 2, 3).toSeq)
    val result3 = RestrictedSelectable.RestrictedFn.apply(Multiplicity.Relevant)(Tuple1(arr))(refs =>
      val size1 = refs._1.size()
      val size2 = refs._1.size()
      val updated = refs._1.write(0, 10)
      Tuple1(updated.freeze())
    )

    assertEquals(result3._1.toSeq, Array(10, 2, 3).toSeq)
  }


  // ============================================================================
  // Tests for @repeatable annotation
  // ============================================================================

  test("@repeatable: method chaining works") {
    val arr = MArray[String](new Array[String](3))

    val result1 = RestrictedSelectable.RestrictedFn.apply(Multiplicity.Affine)(Tuple1(arr))(refs =>
      val afterWrites = refs._1
        .write(0, "a")  // @repeatable
        .write(1, "b")
        .write(2, "c")
      val frozen = afterWrites.freeze()
      Tuple1(frozen)
    )

    assertEquals(result1._1.toList, List("a", "b", "c"))
    val result2 = RestrictedSelectable.RestrictedFn.apply(Multiplicity.Linear)(Tuple1(arr))(refs =>
      val afterWrites = refs._1
        .write(0, "a")  // @repeatable
        .write(1, "b")
        .write(2, "c")
      val frozen = afterWrites.freeze()
      Tuple1(frozen)
    )

    assertEquals(result2._1.toList, List("a", "b", "c"))
    val result3 = RestrictedSelectable.RestrictedFn.apply(Multiplicity.Relevant)(Tuple1(arr))(refs =>
      val afterWrites = refs._1
        .write(0, "a")  // @repeatable
        .write(1, "b")
        .write(2, "c")
      val frozen = afterWrites.freeze()
      Tuple1(frozen)
    )

    assertEquals(result3._1.toList, List("a", "b", "c"))
  }

  test("@unconsumed: NEGATIVE - repeatable methods cannot be called on consumed value") {
    val obtained1 = compileErrors(
      """
      import MArrayOps.*
      import linearfn.{RestrictedSelectable, Multiplicity}

      val arr = MArray[Int](Array(1, 2, 3))

      RestrictedSelectable.RestrictedFn.apply(Multiplicity.Affine)(Tuple1(arr))(refs =>
        val frozen = refs._1.freeze()  // @consumed
        val updated = frozen.write(0, 10)  // Error: write is not @unconsumed
        Tuple1(updated)
      )
    """)

    assert(
      obtained1.contains(TestUtils.missingField),
      s"Expected missing method error but got: $obtained1"
    )
    val obtained2 = compileErrors(
      """
      import MArrayOps.*
      import linearfn.{RestrictedSelectable, Multiplicity}

      val arr = MArray[Int](Array(1, 2, 3))

      RestrictedSelectable.RestrictedFn.apply(Multiplicity.Linear)(Tuple1(arr))(refs =>
        val frozen = refs._1.freeze()  // @consumed
        val updated = frozen.write(0, 10)  // Error: write is not @unconsumed
        Tuple1(updated)
      )
    """)

    assert(
      obtained2.contains(TestUtils.missingField),
      s"Expected missing method error but got: $obtained2"
    )
    val obtained3 = compileErrors(
      """
      import MArrayOps.*
      import linearfn.{RestrictedSelectable, Multiplicity}

      val arr = MArray[Int](Array(1, 2, 3))

      RestrictedSelectable.RestrictedFn.apply(Multiplicity.Relevant)(Tuple1(arr))(refs =>
        val frozen = refs._1.freeze()  // @consumed
        val updated = frozen.write(0, 10)  // Error: write is not @unconsumed
        Tuple1(updated)
      )
    """)

    assert(
      obtained3.contains(TestUtils.missingField),
      s"Expected missing method error but got: $obtained3"
    )
  }

  // ============================================================================
  // Tests for interaction between Multiplicity and annotations
  // ============================================================================

  test("Linear + @consumed: enforces consumption") {
    val arr = MArray[Int](Array(1, 2, 3))

    val result = RestrictedSelectable.RestrictedFn.apply(Multiplicity.Linear)(Tuple1(arr))(refs =>
      val frozen = refs._1.freeze()
      Tuple1(frozen)
    )

    assertEquals(result._1.toList, List(1, 2, 3))
  }

  test("Affine + @unconsumed: allows non-consuming operations") {
    val arr = MArray[Int](Array(1, 2, 3))

    val result = RestrictedSelectable.RestrictedFn.apply(Multiplicity.Affine)(Tuple1(arr))(refs =>
      val size = refs._1.size()  // @unconsumed
      Tuple1(refs._1)
    )

    assertEquals(result._1.freeze().toList, List(1, 2, 3))
  }

  test("Relevant + @repeatable: can use multiple times in chain") {
    val arr = MArray[String](new Array[String](2))

    val result = RestrictedSelectable.RestrictedFn.apply(Multiplicity.Relevant)(Tuple1(arr))(refs =>
      val written = refs._1.write(0, "x").write(1, "y")
      val frozen = written.freeze()  // Relevant requires consumption
      Tuple1(frozen)
    )

    assertEquals(result._1.toList, List("x", "y"))
  }

  test("Linear + @repeatable: method chaining works") {
    val arr = MArray[String](new Array[String](3))

    val result = RestrictedSelectable.RestrictedFn.apply(Multiplicity.Linear)(Tuple1(arr))(refs =>
      val afterWrites = refs._1
        .write(0, "a")  // @repeatable
        .write(1, "b")
        .write(2, "c")
      val frozen = afterWrites.freeze()  // Must consume for Linear
      Tuple1(frozen)
    )

    assertEquals(result._1.toList, List("a", "b", "c"))
  }

  test("Linear + @repeatable: NEGATIVE - must consume after chaining") {
    val obtained = compileErrors("""
      import MArrayOps.*
      import linearfn.{RestrictedSelectable, Multiplicity}

      val arr = MArray[String](new Array[String](2))

      RestrictedSelectable.RestrictedFn.apply(Multiplicity.Linear)(Tuple1(arr))(refs =>
        val afterWrites = refs._1.write(0, "a").write(1, "b")
        Tuple1(afterWrites)  // Error: Linear requires consumption
      )
    """)

    assert(
      obtained.contains(TestUtils.multiplicityConstraintFailed),
      s"Expected consumption requirement error but got: $obtained"
    )
  }

  // ============================================================================
  // Tests for @consumed with Relevant
  // ============================================================================

  test("Relevant + @consumed: allows returning consumed value") {
    val arr = MArray[Int](Array(1, 2, 3))

    val result = RestrictedSelectable.RestrictedFn.apply(Multiplicity.Relevant)(Tuple1(arr))(refs =>
      val frozen = refs._1.freeze()  // @consumed
      Tuple1(frozen)
    )

    assertEquals(result._1.toList, List(1, 2, 3))
  }

  test("Relevant + @consumed: NEGATIVE - cannot call methods after consumed") {
    val obtained = compileErrors("""
      import MArrayOps.*
      import linearfn.{RestrictedSelectable, Multiplicity}

      val arr = MArray[Int](Array(1, 2, 3))

      RestrictedSelectable.RestrictedFn.apply(Multiplicity.Relevant)(Tuple1(arr))(refs =>
        val frozen = refs._1.freeze()
        val oops = frozen.write(0, 10)  // Error: write after @consumed freeze
        Tuple1(oops)
      )
    """)

    assert(
      obtained.contains(TestUtils.missingField),
      s"Expected missing method error but got: $obtained"
    )
  }

  // ============================================================================
  // Tests for @unconsumed with Linear and Relevant
  // ============================================================================

  test("Linear + @unconsumed: can be called on unconsumed value") {
    val arr = MArray[Int](Array(1, 2, 3))

    val result = RestrictedSelectable.RestrictedFn.apply(Multiplicity.Linear)(Tuple1(arr))(refs =>
      val size = refs._1.size()  // @unconsumed
      val frozen = refs._1.freeze()  // Must consume for Linear
      Tuple1(frozen)
    )

    assertEquals(result._1.toList, List(1, 2, 3))
  }

  test("Linear + @unconsumed: can be called before consumption") {
    val arr = MArray[Int](Array(1, 2, 3))

    val result = RestrictedSelectable.RestrictedFn.apply(Multiplicity.Linear)(Tuple1(arr))(refs =>
      val size1 = refs._1.size()  // @unconsumed
      val size2 = refs._1.size()  // Can call multiple times
      val frozen = refs._1.freeze()  // Must consume for Linear
      Tuple1(frozen)
    )

    assertEquals(result._1.toList, List(1, 2, 3))
  }

  test("Relevant + @unconsumed: can be called on unconsumed value") {
    val arr = MArray[Int](Array(1, 2, 3))

    val result = RestrictedSelectable.RestrictedFn.apply(Multiplicity.Relevant)(Tuple1(arr))(refs =>
      val size = refs._1.size()  // @unconsumed
      val updated = refs._1.write(0, 10)
      val frozen = updated.freeze()  // Relevant requires consumption
      Tuple1(frozen)
    )

    assertEquals(result._1.toSeq, Array(10, 2, 3).toSeq)
  }

  test("Relevant + @unconsumed: can be called multiple times") {
    val arr = MArray[Int](Array(1, 2, 3))

    val result = RestrictedSelectable.RestrictedFn.apply(Multiplicity.Relevant)(Tuple1(arr))(refs =>
      val size1 = refs._1.size()  // @unconsumed
      val size2 = refs._1.size()  // Can call multiple times
      val updated = refs._1.write(0, 10)
      val frozen = updated.freeze()  // Relevant requires consumption
      Tuple1(frozen)
    )

    assertEquals(result._1.toSeq, Array(10, 2, 3).toSeq)
  }

  // ============================================================================
  // Tests for correct dependency tracking
  // ============================================================================

  test("Chaining preserves linear usage") {
    val arr = MArray[String](new Array[String](3))

    val result = RestrictedSelectable.RestrictedFn.apply(Multiplicity.Affine)(Tuple1(arr))(refs =>
      // Long chain of @repeatable operations followed by @consumed
      val afterWrites = refs._1
        .write(0, "a")
        .write(1, "b")
        .write(2, "c")
      val frozen = afterWrites.freeze()
      Tuple1(frozen)
    )

    assertEquals(result._1.toList, List("a", "b", "c"))
  }

  test("Multiple arguments with independent operations") {
    val arr1 = MArray[Int](Array(1, 2))
    val arr2 = MArray[Int](Array(3, 4))

    val result = RestrictedSelectable.RestrictedFn.apply(Multiplicity.Affine)((arr1, arr2))(refs =>
      val updated1 = refs._1.write(0, 10)
      val updated2 = refs._2.write(0, 30)
      (updated1, updated2)  // Each arg used exactly once
    )

    assertEquals(result._1.freeze().toList, List(10, 2))
    assertEquals(result._2.freeze().toList, List(30, 4))
  }
