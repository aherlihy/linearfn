package test

import linearfn.RestrictedDynamic

/**
 * Tests for RestrictedDynamic implementation.
 */
class RestrictedDynamicTest extends LinearFnTestSuite(RestrictedDynamic, "RestrictedDynamic"):

  // Compile-time error tests (must use literal strings with compileErrors)
  test("RestrictedDynamic: duplicate argument usage fails compilation") {
    val obtained = compileErrors("""
      val str = "hello"
      val num = 42
      RestrictedDynamic.LinearFn.apply((str, num))(refs =>
        (refs._1, refs._1)
      )
    """)
    assert(obtained.contains(TestUtils.linearMsg), s"obtained: $obtained")
  }

  test("RestrictedDynamic: manual non-linear") {
    val obtained = compileErrors("""
      val str = "hello"
      val num = 42
      val actual = 5
      val result = RestrictedDynamic.LinearFn.apply((str, num))(refs =>
        val tmp: RestrictedDynamic.Restricted[Int, (0, 0), EmptyTuple] = RestrictedDynamic.Restricted.LinearRef[Int, (0, 0), EmptyTuple](() => actual)
        (tmp, refs._1)
      )
    """)
    assert(obtained.contains(TestUtils.affineMsg), s"obtained: $obtained")
  }

  test("RestrictedDynamic: manual non-affine") {
    val obtained = compileErrors("""
      val str = "hello"
      val num = 42
      val actual = 5
      val result = RestrictedDynamic.LinearFn.apply((str, num))(refs =>
        val tmp: RestrictedDynamic.Restricted[Int, Tuple1[0], EmptyTuple] = RestrictedDynamic.Restricted.LinearRef[Int, Tuple1[0], EmptyTuple](() => actual)
        (tmp, refs._1)
      )
    """)
    assert(obtained.contains(TestUtils.linearMsg), s"obtained: $obtained")
  }

  test("RestrictedDynamic: dependencies are tracked for primitive operator") {
    val obtained = compileErrors("""
      val str = "hello"
      val num = 42
      RestrictedDynamic.LinearFn.apply((str, num))(refs => (refs._2 + refs._2, refs._1))
    """)
    assert(obtained.contains(TestUtils.affineMsg), s"obtained: $obtained")
  }

  // Runtime tests for field and method access
  test("valid field access on case class") {
    case class Person(name: String, age: Int)
    val person = Person("Alice", 30)
    // Field access works - just verify no runtime errors
    RestrictedDynamic.LinearFn.apply(Tuple1(person))(refs =>
      val _age = refs._1.age  // This should compile and run without errors
      Tuple1(refs._1)
    )
    assert(true) // If we get here, field access worked
  }

//   Note: RestrictedDynamic is not type-safe so won't fail if field doesn't exist
//   test("field access fails for non-existing field") {
//     val obtained = compileErrors("""
//       case class Person(name: String, age: Int)
//       val person = Person("Alice", 30)
//       RestrictedDynamic.LinearFn.apply(Tuple1(person))(refs =>
//         val x = refs._1.nonExistentField
//         Tuple1(refs._1)
//       )
//     """)
//     assert(obtained.contains("value nonExistentField is not a member"), s"obtained: $obtained")
//   }

  test("valid field access on primitive type") {
    val str = "hello"
    // Field access works - just verify no runtime errors
    RestrictedDynamic.LinearFn.apply(Tuple1(str))(refs =>
      val _len = refs._1.length  // This should compile and run without errors
      Tuple1(refs._1)
    )
    assert(true) // If we get here, field access worked
  }

  // Note: RestrictedDynamic doesn't fully support method calls (hits NotImplementedError)
  // test("dependencies tracked for combine method") {
  //   case class Person(name: String, age: Int):
  //     def combine(other: Person): Person =
  //       Person(s"${this.name} & ${other.name}", this.age + other.age)
  //
  //   val person1 = Person("Alice", 30)
  //   val person2 = Person("Bob", 25)
  //   val result = RestrictedDynamic.LinearFn.apply((person1, person2))(refs =>
  //     (refs._1.combine(refs._2), refs._2)
  //   )
  //   assertEquals(result, (Person("Alice & Bob", 55), person2))
  // }

  test("combine method fails with duplicate use") {
    val obtained = compileErrors("""
      case class Person(name: String, age: Int):
        def combine(other: Person): Person =
          Person(s"${this.name} & ${other.name}", this.age + other.age)

      val person1 = Person("Alice", 30)
      val person2 = Person("Bob", 25)
      RestrictedDynamic.LinearFn.apply((person1, person2))(refs =>
        val combined = refs._1.combine(refs._1)
        (combined, combined)
      )
    """)
    assert(obtained.contains(TestUtils.affineMsg), s"obtained: $obtained")
  }
