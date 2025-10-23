package test

import munit.FunSuite
import linearfn.{RestrictedSelectable, ops, consumed}
import scala.annotation.experimental

@experimental
class ConsumedAnnotationTest extends FunSuite:
  import TestUtils.*

  test("apply allows returning consumed value") {
    import MArrayOps.*

    val arr = MArray[Int](Array(1, 2, 3))

    val result = RestrictedSelectable.LinearFn.apply(Tuple1(arr))(refs =>
      val updated = refs._1.write(0, 10)
      val frozen = updated.freeze()
      Tuple1(frozen)
    )

    assertEquals(result._1.toList, List(10, 2, 3))
  }

  test("apply allows returning unconsumed value") {
    import MArrayOps.*

    val arr = MArray[Int](Array(1, 2, 3))

    val result = RestrictedSelectable.LinearFn.apply(Tuple1(arr))(refs =>
      val updated = refs._1.write(0, 10)
      Tuple1(updated)
    )

    assertEquals(result._1.freeze().toSeq, Array(10, 2, 3).toSeq)
  }

  test("apply does not allow returning both consumed and unconsumed for the same reference") {
    val obtained = compileErrors("""
      import MArrayOps.*
      import linearfn.RestrictedSelectable

      val arr = MArray[Int](Array(1, 2, 3))

      RestrictedSelectable.LinearFn.apply(Tuple1(arr))(refs =>
        val frozen = refs._1.freeze()
        (refs._1, frozen)  // Error: returning both original and consumed
      )
    """)

    assert(
      obtained.contains(argsMsg),
      s"Expected type error but got: $obtained"
    )
  }

  test("applyConsumed does not allow returning both consumed and unconsumed for the same reference") {
    val obtained = compileErrors(
      """
      import MArrayOps.*
      import linearfn.RestrictedSelectable

      val arr = MArray[Int](Array(1, 2, 3))

      RestrictedSelectable.LinearFn.apply(Tuple1(arr))(refs =>
        val frozen = refs._1.freeze()
        (refs._1, frozen)  // Error: returning both original and consumed
      )
    """)

    assert(
      obtained.contains(argsMsg),
      s"Expected type error but got: $obtained"
    )
  }

  test("applyConsumed requires all arguments to be consumed") {
    import MArrayOps.*

    val arr = MArray[Int](Array(1, 2, 3))

    // This should work because freeze() consumes the array
    val result = RestrictedSelectable.LinearFn.applyConsumed(Tuple1(arr))(refs =>
      val frozen = refs._1.freeze()
      Tuple1(frozen)
    )

    assertEquals(result._1.toList, List(1, 2, 3))
  }

  test("applyConsumed fails if not all arguments are consumed") {
    // This should fail to compile because write() doesn't consume
    val obtained = compileErrors("""
      import MArrayOps.*
      import linearfn.RestrictedSelectable

      val arr = MArray[Int](Array(1, 2, 3))

      RestrictedSelectable.LinearFn.applyConsumed(Tuple1(arr))(refs =>
        val updated = refs._1.write(0, 10)
        Tuple1(updated)
      )
    """)

    assert(
      obtained.contains(consumptionExactlyOneMsg),
      s"Expected consumption requirement error but got: $obtained"
    )
  }

  test("applyConsumed fails consumed called twice") {
    val obtained = compileErrors(
      """
      import MArrayOps.*
      import linearfn.RestrictedSelectable

      val arr = MArray[Int](Array(1, 2, 3))

      RestrictedSelectable.LinearFn.applyConsumed(Tuple1(arr))(refs =>
        val updated = refs._1.freeze().freeze()
        Tuple1(updated)
      )
    """)

    assert(
      obtained.contains(argsMsg),
      s"Expected type error but got: $obtained"
    )
  }

  test("apply fails consumed called twice") {
    val obtained = compileErrors(
      """
      import MArrayOps.*
      import linearfn.RestrictedSelectable

      val arr = MArray[Int](Array(1, 2, 3))

      RestrictedSelectable.LinearFn.apply(Tuple1(arr))(refs =>
        val updated = refs._1.freeze().freeze()
        Tuple1(updated)
      )
    """)

    assert(
      obtained.contains(argsMsg),
      s"Expected type error but got: $obtained"
    )
  }

  test("Multiple consumed values are allowed if different references") {
    import MArrayOps.*

    val arr1 = MArray[Int](Array(1, 2, 3))
    val arr2 = MArray[Int](Array(4, 5, 6))

    val result = RestrictedSelectable.LinearFn.applyConsumed((arr1, arr2))(refs =>
      val frozen1 = refs._1.freeze()
      val frozen2 = refs._2.freeze()
      (frozen1, frozen2)
    )

    assertEquals(result._1.toList, List(1, 2, 3))
    assertEquals(result._2.toList, List(4, 5, 6))
  }

  test("Chaining operations before consuming") {
    import MArrayOps.*

    val arr = MArray[String](new Array[String](3))

    val result = RestrictedSelectable.LinearFn.apply(Tuple1(arr))(refs =>
      val afterWrites = refs._1
        .write(0, "a")
        .write(1, "b")
        .write(2, "c")
      val frozen = afterWrites.freeze()
      Tuple1(frozen)
    )

    assertEquals(result._1.toList, List("a", "b", "c"))
  }
