package test

import munit.FunSuite
import scala.annotation.experimental
import linearfn.{Multiplicity}
import linearfn.RestrictedSelectable.{given, *}


/**
 * Tests demonstrating the implicit conversion from plain values to Restricted types.
 */
@experimental
class ImplicitConversionTest extends FunSuite:
  import OpsExampleOps.*

  test("implicit conversion allows passing plain String to extension method") {
    val person = OpsExample("Alice", "30")

    val result = RestrictedFn.apply(Multiplicity.Linear)(Tuple1(person))(refs =>
      // Can pass plain String - implicitly converted to Restricted[String, EmptyTuple]
      val updated = refs._1.singleRestrictedPrimitiveArg("Alicia")
      ForAllLinearConnective(Tuple1(updated))
    )

    assertEquals(result._1.name, "Alicia")
  }

  test("implicit conversion allows mixing plain and Restricted values") {
    val person1 = OpsExample("Alice", "30")
    val person2 = OpsExample("Bob", "25")

    val result = RestrictedFn.apply(Multiplicity.Linear)((person1, person2))(refs =>
      // refs._2 is already Restricted, but singleRestrictedProductArg accepts both Restricted and plain
      val combined = refs._1.singleRestrictedProductArg(refs._2)
      ForAllLinearConnective(Tuple1(combined))
    )

    assertEquals(result._1.name, "Alice & Bob")
  }

  test("plain values have EmptyTuple dependencies - don't affect type checking") {
    val person = OpsExample("Alice", "30")

    val result = RestrictedFn.apply(Multiplicity.Linear)(Tuple1(person))(refs =>
      // Plain "Alicia" gets converted to Restricted[String, EmptyTuple]
      // Tuple.Concat[EmptyTuple, D] = D, so the result type is still correct
      val updated = refs._1.singleRestrictedPrimitiveArg("Alicia")
      ForAllLinearConnective(Tuple1(updated))
    )

    assertEquals(result._1.name, "Alicia")
  }

  test("multiple plain string arguments are all tracked independently") {
    case class Person(name: String):
      def greetWith(prefix: String, suffix: String): String =
        s"$prefix $name $suffix"

    extension [D <: Tuple](p: Restricted[Person, D])
      def greetWith[D1 <: Tuple, D2 <: Tuple](
        prefix: Restricted[String, D1],
        suffix: Restricted[String, D2]
      ): Restricted[String, Tuple.Concat[D1, Tuple.Concat[D2, D]]] =
        p.stageCall[String, Tuple.Concat[D1, Tuple.Concat[D2, D]]]("greetWith", (prefix, suffix))

    val person = Person("Alice")

    val result = RestrictedFn.apply(Multiplicity.Linear)(Tuple1(person))(refs =>
      // Both plain strings - each converted to Restricted[String, EmptyTuple]
      val greeting = refs._1.greetWith("Hello", "there!")
      ForAllLinearConnective(Tuple1(greeting))
    )

    assertEquals(result._1, "Hello Alice there!")
  }
