package linearfn

import munit.FunSuite
import scala.annotation.experimental
import OpsExampleOps.*

/**
 * Tests demonstrating the implicit conversion from plain values to Restricted types.
 */
@experimental
class ImplicitConversionTest extends FunSuite:

  test("implicit conversion allows passing plain String to extension method") {
    val person = OpsExample("Alice", "30")

    val result = RestrictedSelectable.LinearFn.apply(Tuple1(person))(refs =>
      // Can pass plain String - implicitly converted to Restricted[String, EmptyTuple]
      val updated = refs._1.singleRestrictedPrimitiveArg("Alicia")
      Tuple1(updated)
    )

    assertEquals(result._1.name, "Alicia")
  }

  test("implicit conversion allows mixing plain and Restricted values") {
    val person1 = OpsExample("Alice", "30")
    val person2 = OpsExample("Bob", "25")

    val result = RestrictedSelectable.LinearFn.apply((person1, person2))(refs =>
      // refs._2 is already Restricted, but singleRestrictedProductArg accepts both Restricted and plain
      val combined = refs._1.singleRestrictedProductArg(refs._2)
      (combined, combined)
    )

    assertEquals(result._1.name, "Alice & Bob")
  }

  test("plain values have EmptyTuple dependencies - don't affect type checking") {
    val person = OpsExample("Alice", "30")

    val result = RestrictedSelectable.LinearFn.apply(Tuple1(person))(refs =>
      // Plain "Alicia" gets converted to Restricted[String, EmptyTuple]
      // Tuple.Concat[EmptyTuple, D] = D, so the result type is still correct
      val updated = refs._1.singleRestrictedPrimitiveArg("Alicia")
      Tuple1(updated)
    )

    assert(result._1.name == "Alicia")
  }

  test("multiple plain string arguments are all tracked independently") {
    case class Person(name: String):
      def greetWith(prefix: String, suffix: String): String =
        s"$prefix $name $suffix"

    extension [D <: Tuple](p: RestrictedSelectable.Restricted[Person, D])
      def greetWith[D1 <: Tuple, D2 <: Tuple](
        prefix: RestrictedSelectable.Restricted[String, D1],
        suffix: RestrictedSelectable.Restricted[String, D2]
      ): RestrictedSelectable.Restricted[String, Tuple.Concat[D1, Tuple.Concat[D2, D]]] =
        p.stageCall[String, Tuple.Concat[D1, Tuple.Concat[D2, D]]]("greetWith", (prefix, suffix))

    val person = Person("Alice")

    val result = RestrictedSelectable.LinearFn.apply(Tuple1(person))(refs =>
      // Both plain strings - each converted to Restricted[String, EmptyTuple]
      val greeting = refs._1.greetWith("Hello", "there!")
      Tuple1(greeting)
    )

    assertEquals(result._1, "Hello Alice there!")
  }
