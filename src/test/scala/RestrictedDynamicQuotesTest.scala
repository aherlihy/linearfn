package test


import linearfn.{RestrictedDynamicQuotes}

/**
 * Tests for RestrictedDynamicQuotes implementation.
 */
class RestrictedDynamicQuotesTest extends RestrictedFnTestSuite(RestrictedDynamicQuotes, "RestrictedDynamicQuotes"):

  // Compile-time error tests (must use literal strings with compileErrors)
  test("RestrictedDynamicQuotes: duplicate argument usage fails compilation") {
    val obtained = compileErrors("""
      val str = "hello"
      val num = 42
      RestrictedDynamicQuotes.RestrictedFn.apply((str, num))(refs =>
        (refs._1, refs._1)
      )
    """)
    assert(obtained.contains(TestUtils.horizontalRelevanceFailed), s"obtained: $obtained")
  }

  test("RestrictedDynamicQuotes: manual non-linear") {
    val obtained = compileErrors("""
      val str = "hello"
      val num = 42
      val actual = 5
      val result = RestrictedDynamicQuotes.RestrictedFn.apply((str, num))(refs =>
        val tmp: RestrictedDynamicQuotes.Restricted[Int, (0, 0), EmptyTuple] = RestrictedDynamicQuotes.Restricted.RestrictedRef[Int, (0, 0), EmptyTuple](() => actual)
        (tmp, refs._1)
      )
    """)
    assert(obtained.contains(TestUtils.horizontalRelevanceFailed), s"obtained: $obtained")
  }

  test("RestrictedDynamicQuotes: manual non-affine") {
    val obtained = compileErrors("""
      val str = "hello"
      val num = 42
      val actual = 5
      val result = RestrictedDynamicQuotes.RestrictedFn.apply((str, num))(refs =>
        val tmp: RestrictedDynamicQuotes.Restricted[Int, Tuple1[0], EmptyTuple] = RestrictedDynamicQuotes.Restricted.RestrictedRef[Int, Tuple1[0], EmptyTuple](() => actual)
        (tmp, refs._1)
      )
    """)
    assert(obtained.contains(TestUtils.horizontalRelevanceFailed), s"obtained: $obtained")
  }

//  FAILS: Quotes uses Select.unique which doesn't work on overloaded methods
//   test("RestrictedDynamicQuotes: dependencies are tracked for primitive operator") {
//     val obtained = compileErrors("""
//       val str = "hello"
//       val num = 42
//       RestrictedDynamicQuotes.RestrictedFn.apply((str, num))(refs => (refs._2 + refs._2, refs._1))
//     """)
//     assert(obtained.contains(TestUtils.horizontalRelevanceFailed), s"obtained: $obtained")
//   }

  // Runtime tests for field and method access
  test("valid field access on case class") {
    case class Person(name: String, age: Int)
    val person = Person("Alice", 30)
    // Field access works - just verify no runtime errors
    RestrictedDynamicQuotes.RestrictedFn.apply(Tuple1(person))(refs =>
      val _age = refs._1.age  // This should compile and run without errors
      Tuple1(refs._1)
    )
    assert(true) // If we get here, field access worked
  }

  test("field access fails for non-existing field") {
    val obtained = compileErrors("""
      case class Person(name: String, age: Int)
      val person = Person("Alice", 30)
      RestrictedDynamicQuotes.RestrictedFn.apply(Tuple1(person))(refs =>
        val x = refs._1.nonExistentField
        Tuple1(refs._1)
      )
    """)
    assert(obtained.contains("value nonExistentField is not a member"), s"obtained: $obtained")
  }

// FAILS:  Expected an expression. This is a partially applied Term. Try eta-expanding the term first.
//   test("valid field access on primitive type") {
//     val str = "hello"
//     val result = RestrictedDynamicQuotes.RestrictedFn.apply(Tuple1(str))(refs =>
//       val len = refs._1.length
//       (refs._1, len)
//     )
//     assertEquals(result, (str, 5))
//   }

  // FAILS: bug
//   test("dependencies tracked for combine method") {
//     case class Person(name: String, age: Int):
//       def combine(other: Person): Person =
//         Person(s"${this.name} & ${other.name}", this.age + other.age)
//
//     val person1 = Person("Alice", 30)
//     val person2 = Person("Bob", 25)
//     val result = RestrictedDynamicQuotes.RestrictedFn.apply((person1, person2))(refs =>
//       val combined = refs._1.combine(refs._2)
//       (combined, refs._2)
//     )
//     assertEquals(result, (Person("Alice & Bob", 55), person2))
//   }

  test("combine method fails with duplicate use") {
    val obtained = compileErrors("""
      case class Person(name: String, age: Int):
        def combine(other: Person): Person =
          Person(s"${this.name} & ${other.name}", this.age + other.age)

      val person1 = Person("Alice", 30)
      val person2 = Person("Bob", 25)
      RestrictedDynamicQuotes.RestrictedFn.apply((person1, person2))(refs =>
        val combined = refs._1.combine(refs._1)
        (combined, combined)
      )
    """)
    assert(obtained.contains(TestUtils.horizontalRelevanceFailed), s"obtained: $obtained")
  }