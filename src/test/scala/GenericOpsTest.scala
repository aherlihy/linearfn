package test


import munit.FunSuite
import scala.annotation.experimental
import linearfn.{ops, repeatable, RestrictedSelectable}

// Simple generic class to test generic type parameter support
@ops
case class Box[T](value: T):
  @repeatable
  def get(): T = value
  @repeatable
  def set(newValue: T): Box[T] = Box(newValue)

/**
 * Tests for @ops annotation with generic type parameters.
 * Tests both the generator (compile-time) and runtime reflection with primitives.
 */
@experimental
class GenericOpsTest extends FunSuite:
  import OpsExampleOps.*
  import BoxOps.*
  import MArrayOps.*

  // ===== Generic class tests =====

  test("generic Box[String] - get and set work") {
    val box = Box[String]("hello")

    val result = RestrictedSelectable.LinearFn.apply(Tuple1(box))(refs =>
      val updated = refs._1.set("world")
      val value = updated.get()
      Tuple1(value)
    )

    assertEquals(result._1, "world")
  }

  test("generic Box[Int] - works with primitives after boxing fix") {
    val box = Box[Int](42)

    val result = RestrictedSelectable.LinearFn.apply(Tuple1(box))(refs =>
      val updated = refs._1.set(100)
      val value = updated.get()
      Tuple1(value)
    )

    assertEquals(result._1, 100)
  }

  // ===== MArray tests with primitives =====

  test("MArray[Int] - write with primitives works after boxing fix") {
    val arr = MArray[Int](new Array[Int](3))

    val result = RestrictedSelectable.LinearFn.apply(Tuple1(arr))(refs =>
      val after1 = refs._1.write(0, 10)
      val after2 = after1.write(1, 20)
      val after3 = after2.write(2, 30)
      val frozen = after3.freeze()
      Tuple1(frozen)
    )

    assertEquals(result._1.toList, List(10, 20, 30))
  }

  test("MArray[String] - works with reference types") {
    val arr = MArray[String](new Array[String](2))

    val result = RestrictedSelectable.LinearFn.apply(Tuple1(arr))(refs =>
      val after1 = refs._1.write(0, "hello")
      val after2 = after1.write(1, "world")
      val frozen = after2.freeze()
      Tuple1(frozen)
    )

    assertEquals(result._1.toList, List("hello", "world"))
  }

  test("MArray[Int] - read returns tuple with primitives") {
    val arr = MArray[Int](Array(100, 200, 300))

    val result = RestrictedSelectable.LinearFn.apply(Tuple1(arr))(refs =>
      val readResult = refs._1.read(1)
      Tuple1(readResult)
    )

    val (returnedArr, readValue) = result._1
    assertEquals(returnedArr.freeze().toList, List(100, 200, 300))
    assertEquals(readValue, 200)
  }

  test("MArray[Int] - chaining writes with primitives") {
    val arr = MArray[Int](new Array[Int](3))

    val result = RestrictedSelectable.LinearFn.apply(Tuple1(arr))(refs =>
      val final_arr = refs._1
        .write(0, 1)
        .write(1, 2)
        .write(2, 3)
      Tuple1(final_arr.freeze())
    )

    assertEquals(result._1.toList, List(1, 2, 3))
  }

  test("MArray[OpsExample] - generics with user-defined types") {
    val arr = MArray[OpsExample](new Array[OpsExample](2))
    val ex1 = OpsExample("Alice", "30")
    val ex2 = OpsExample("Bob", "25")

    val result = RestrictedSelectable.LinearFn.apply(Tuple1(arr))(refs =>
      val after1 = refs._1.write(0, ex1)
      val after2 = after1.write(1, ex2)
      Tuple1(after2.freeze())
    )

    assertEquals(result._1(0).name, "Alice")
    assertEquals(result._1(1).name, "Bob")
  }

  // ===== Linearity violation tests =====

  test("generic Box - linearity violation caught at compile time") {
    val obtained = compileErrors("""
      import linearfn.RestrictedSelectable
      import _root_.test.{Box, BoxOps}
      import BoxOps.*

      val box = Box[String]("test")
      RestrictedSelectable.LinearFn.strictApply(Tuple1(box))(refs =>
        val v1 = refs._1.get()
        val v2 = refs._1.get()  // Error: returning 2 values from 1 input with strictApply
        (v1, v2)
      )
    """)
    assert(
      obtained.contains(TestUtils.strictFnFailed),
      s"Expected linearity error but got: $obtained"
    )
  }

  // ===== Type safety tests =====

  test("MArray[Int] - passing String instead of Int fails at compile time") {
    val obtained = compileErrors("""
      import linearfn.RestrictedSelectable
      import _root_.test.{MArray, MArrayOps}
      import MArrayOps.*

      val arr = MArray[Int](new Array[Int](3))
      RestrictedSelectable.LinearFn.apply(Tuple1(arr))(refs =>
        val after = refs._1.write(0, "not an int")  // Error: String is not Int
        Tuple1(after.freeze())
      )
    """)
    assert(
      obtained.contains("Found:") && obtained.contains("Required:"),
      s"Expected type error but got: $obtained"
    )
  }

  test("Box[String] - passing Int instead of String fails at compile time") {
    val obtained = compileErrors("""
      import linearfn.RestrictedSelectable
      import _root_.test.{Box, BoxOps}
      import BoxOps.*

      val box = Box[String]("hello")
      RestrictedSelectable.LinearFn.apply(Tuple1(box))(refs =>
        val updated = refs._1.set(42)  // Error: Int is not String
        Tuple1(updated.get())
      )
    """)
    assert(
      obtained.contains("Found:") && obtained.contains("Required:"),
      s"Expected type error but got: $obtained"
    )
  }

  test("MArray[String] - passing MArray[Int] to function expecting MArray[String] fails") {
    val obtained = compileErrors("""
      import linearfn.RestrictedSelectable
      import _root_.test.{MArray, MArrayOps}
      import MArrayOps.*

      val intArr = MArray[Int](new Array[Int](3))
      val stringArr = MArray[String](new Array[String](3))

      // Try to use intArr where stringArr is expected
      RestrictedSelectable.LinearFn.apply(Tuple1(intArr))(refs =>
        val after = refs._1.write(0, "string")  // Error: can't write String to MArray[Int]
        Tuple1(after.freeze())
      )
    """)
    assert(
      obtained.contains("Found:") && obtained.contains("Required:"),
      s"Expected type error but got: $obtained"
    )
  }
