package test

import munit.FunSuite
import linearfn.{RestrictedSelectable, ops, consumed, unconsumed}
import scala.annotation.experimental

@experimental
class UnconsumedAnnotationTest extends FunSuite:


  test("@unconsumed size() can be called on unconsumed value") {
    import MArrayOps.*

    val arr = MArray[Int](Array(1, 2, 3))

    val result = RestrictedSelectable.RestrictedFn.apply(Tuple1(arr))(refs =>
      val sz = refs._1.size()
      Tuple1(refs._1)  // Return the array, size() didn't consume it
    )

    assertEquals(result._1.freeze().toSeq, Seq(1, 2, 3))
  }

  test("@unconsumed size() can be called on consumed value") {
    import MArrayOps.*

    val arr = MArray[Int](Array(1, 2, 3))

    // Key test: seal() then size()
    val result = RestrictedSelectable.RestrictedFn.apply(Tuple1(arr))(refs =>
      val consumed = refs._1.seal()  // Consumes the array, returns MArray
      val sz = consumed.size()       // size() works on consumed value!
      Tuple1(consumed)
    )
    assertEquals(result._1.freeze().toSeq, Seq(1, 2, 3))
  }

  test("@unconsumed nothing() can be chained before and after seal") {
    import MArrayOps.*

    val arr = MArray[Int](Array(1, 2, 3))

    // Test: nothing().seal().nothing()
    val result = RestrictedSelectable.RestrictedFn.apply(Tuple1(arr))(refs =>
      val arr1 = refs._1.nothing()   // @unconsumed on unconsumed
      val consumed = arr1.seal()     // @consumed on unconsumed
      val arr2 = consumed.nothing()  // @unconsumed on consumed!
      Tuple1(arr2)
    )
    assertEquals(result._1.freeze().toSeq, Seq(1, 2, 3))
  }

  test("multiple @unconsumed calls preserve state") {
    import MArrayOps.*

    val arr = MArray[Int](Array(1, 2, 3))

    // Chain multiple nothing() calls before and after seal
    val result = RestrictedSelectable.RestrictedFn.apply(Tuple1(arr))(refs =>
      val arr1 = refs._1.nothing().nothing().nothing()
      val consumed = arr1.seal()
      val arr2 = consumed.nothing().nothing()
      Tuple1(arr2)
    )

    assertEquals(result._1.freeze().toSeq, Seq(1, 2, 3))
  }

  test("default method cannot be called on consumed value") {
    // write() is not @unconsumed, so it requires EmptyTuple
    val obtained = compileErrors("""
      import MArrayOps.*
      import linearfn.RestrictedSelectable

      val arr = MArray[Int](Array(1, 2, 3))

      RestrictedSelectable.RestrictedFn.apply(Tuple1(arr))(refs =>
        val frozen = refs._1.freeze()     // Consumes
        val updated = frozen.write(0, 10) // Error: write requires unconsumed
        Tuple1(updated)
      )
    """)

    assert(
      obtained.contains(TestUtils.missingField),
      s"Expected type error but got: $obtained"
    )
  }

  test("@consumed method cannot be called after @unconsumed on consumed value") {
    val obtained = compileErrors("""
      import MArrayOps.*
      import linearfn.RestrictedSelectable

      val arr = MArray[Int](Array(1, 2, 3))

      RestrictedSelectable.RestrictedFn.apply(Tuple1(arr))(refs =>
        val frozen = refs._1.freeze()     // Consumes: C = Tuple1[true]
        val arr1 = frozen.nothing()       // Preserves: C = Tuple1[true]
        val frozen2 = arr1.freeze()       // Error: freeze requires C = EmptyTuple
        Tuple1(frozen2)
      )
    """)

    assert(
      obtained.contains(TestUtils.missingField),
      s"Expected type error but got: $obtained"
    )
  }

  test("mix of default, @consumed, and @unconsumed methods") {
    import MArrayOps.*

    val arr = MArray[Int](Array(1, 2, 3))

    val result = RestrictedSelectable.RestrictedFn.apply(Tuple1(arr))(refs =>
      val arr1 = refs._1.nothing()       // @unconsumed on unconsumed
      val arr2 = arr1.write(0, 10)       // default on unconsumed
      val arr3 = arr2.nothing()          // @unconsumed on unconsumed
      val consumed = arr3.seal()         // @consumed on unconsumed
      val arr4 = consumed.nothing()      // @unconsumed on consumed
      Tuple1(arr4)
    )

    assertEquals(result._1.freeze().toSeq, Seq(10, 2, 3))
  }

  test("combine() accepts unconsumed argument") {
    import MArrayOps.*

    val arr1 = MArray[Int](Array(1, 2))
    val arr2 = MArray[Int](Array(3, 4))

    // Both receiver and argument are unconsumed
    val result = RestrictedSelectable.RestrictedFn.apply((arr1, arr2))(refs =>
      val combined = refs._1.combine(refs._2)
      (combined, combined)  // Return combined twice to match 2 args → 2 returns
    )

    assertEquals(result._1.freeze().toSeq, Seq(1, 2, 3, 4))
    assertEquals(result._2.freeze().toSeq, Seq(1, 2, 3, 4))
  }

  test("combine() accepts consumed argument (current behavior)") {
    import MArrayOps.*

    val arr1 = MArray[Int](Array(1, 2))
    val arr2 = MArray[Int](Array(3, 4))

    // Receiver is unconsumed, argument is consumed
    // NOTE: Currently arguments ignore consumption state - this is accepted
    // Future work: Could require @unconsumed or @consumed on parameters
    val result = RestrictedSelectable.RestrictedFn.apply((arr1, arr2))(refs =>
      val arr2Consumed = refs._2.seal()    // Consume argument
      val combined = refs._1.combine(arr2Consumed)  // Combine accepts consumed arg
      (combined, combined)
    )

    assertEquals(result._1.freeze().toSeq, Seq(1, 2, 3, 4))
    assertEquals(result._2.freeze().toSeq, Seq(1, 2, 3, 4))
  }

  test("@unconsumed combine can be called on consumed receiver") {
    import MArrayOps.*

    val arr1 = MArray[Int](Array(1, 2))
    val arr2 = MArray[Int](Array(3, 4))

    // Receiver is consumed, argument is unconsumed
    // combine() is @unconsumed so it can be called on consumed receiver
    val result = RestrictedSelectable.RestrictedFn.apply((arr1, arr2))(refs =>
      val arr1Consumed = refs._1.seal()    // Consume receiver
      val combined = arr1Consumed.combine(refs._2)  // @unconsumed combine works on consumed!
      (combined, combined)
    )

    assertEquals(result._1.freeze().toSeq, Seq(1, 2, 3, 4))
    assertEquals(result._2.freeze().toSeq, Seq(1, 2, 3, 4))
  }

  test("combine with both receiver and argument consumed") {
    import MArrayOps.*

    val arr1 = MArray[Int](Array(1, 2))
    val arr2 = MArray[Int](Array(3, 4))

    // Both receiver and argument are consumed
    // combine() is @unconsumed so it accepts consumed receiver
    // Arguments currently ignore consumption state, so consumed arg also works
    val result = RestrictedSelectable.RestrictedFn.apply((arr1, arr2))(refs =>
      val arr1Consumed = refs._1.seal()
      val arr2Consumed = refs._2.seal()
      val combined = arr1Consumed.combine(arr2Consumed)  // Both consumed!
      (combined, combined)
    )

    assertEquals(result._1.freeze().toSeq, Seq(1, 2, 3, 4))
    assertEquals(result._2.freeze().toSeq, Seq(1, 2, 3, 4))
  }

  test("combine tracks dependencies correctly") {
    import MArrayOps.*

    val arr1 = MArray[Int](Array(1, 2))
    val arr2 = MArray[Int](Array(3, 4))

    // Verify linearity: can't return both combined and original refs._2
    val obtained = compileErrors("""
      import MArrayOps.*
      import linearfn.RestrictedSelectable

      val arr1 = MArray[Int](Array(1, 2))
      val arr2 = MArray[Int](Array(3, 4))

      RestrictedSelectable.RestrictedFn.apply((arr1, arr2))(refs =>
        val combined = refs._1.combine(refs._1)
        (combined, refs._2)  // Error: refs._2 used in both
      )
    """)

    assert(
      obtained.contains(TestUtils.horizontalAffineFailed),
      s"Expected linearity error but got: $obtained"
    )
  }
