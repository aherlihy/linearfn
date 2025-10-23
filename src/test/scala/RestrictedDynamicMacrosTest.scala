package test

import linearfn.RestrictedDynamicMacros

/**
 * Tests for RestrictedDynamicMacros implementation.
 */
class RestrictedDynamicMacrosTest extends LinearFnTestSuite(RestrictedDynamicMacros, "RestrictedDynamicMacros"):

  // Compile-time error tests (must use literal strings with compileErrors)
  test("RestrictedDynamicMacros: duplicate argument usage fails compilation") {
    val obtained = compileErrors("""
      val str = "hello"
      val num = 42
      RestrictedDynamicMacros.LinearFn.apply((str, num))(refs =>
        (refs._1, refs._1)
      )
    """)
    assert(obtained.contains(TestUtils.linearMsg), s"obtained: $obtained")
  }

  test("RestrictedDynamicMacros: manual non-linear") {
    val obtained = compileErrors("""
      val str = "hello"
      val num = 42
      val actual = 5
      val result = RestrictedDynamicMacros.LinearFn.apply((str, num))(refs =>
        val tmp: RestrictedDynamicMacros.Restricted[Int, (0, 0), EmptyTuple] = RestrictedDynamicMacros.Restricted.LinearRef[Int, (0, 0), EmptyTuple](() => actual)
        (tmp, refs._1)
      )
    """)
    assert(obtained.contains(TestUtils.affineMsg), s"obtained: $obtained")
  }

  test("RestrictedDynamicMacros: manual non-affine") {
    val obtained = compileErrors("""
      val str = "hello"
      val num = 42
      val actual = 5
      val result = RestrictedDynamicMacros.LinearFn.apply((str, num))(refs =>
        val tmp: RestrictedDynamicMacros.Restricted[Int, Tuple1[0], EmptyTuple] = RestrictedDynamicMacros.Restricted.LinearRef[Int, Tuple1[0], EmptyTuple](() => actual)
        (tmp, refs._1)
      )
    """)
    assert(obtained.contains(TestUtils.linearMsg), s"obtained: $obtained")
  }

  test("RestrictedDynamicMacros: dependencies are tracked for primitive operator") {
    val obtained = compileErrors("""
      val str = "hello"
      val num = 42
      RestrictedDynamicMacros.LinearFn.apply((str, num))(refs => (refs._2 + refs._2, refs._1))
    """)
    assert(obtained.contains(TestUtils.affineMsg), s"obtained: $obtained")
  }

  // Runtime tests for field and method access
  test("valid field access on case class") {
    case class Person(name: String, age: Int)
    val person = Person("Alice", 30)
    // Field access works - just verify no runtime errors
    RestrictedDynamicMacros.LinearFn.apply(Tuple1(person))(refs =>
      val _age = refs._1.age  // This should compile and run without errors
      Tuple1(refs._1)
    )
    assert(true) // If we get here, field access worked
  }

  test("field access fails for non-existing field") {
    val obtained = compileErrors("""
      case class Person(name: String, age: Int)
      val person = Person("Alice", 30)
      RestrictedDynamicMacros.LinearFn.apply(Tuple1(person))(refs =>
        val x = refs._1.nonExistentField
        Tuple1(refs._1)
      )
    """)
    assert(obtained.contains("Field 'nonExistentField' not found"), s"obtained: $obtained")
  }

  test("valid field access on primitive type") {
    val str = "hello"
    // Field access works - just verify no runtime errors
    RestrictedDynamicMacros.LinearFn.apply(Tuple1(str))(refs =>
      val _len = refs._1.length  // This should compile and run without errors
      Tuple1(refs._1)
    )
    assert(true) // If we get here, field access worked
  }

  test("dependencies tracked for combine method") {
    case class Person(name: String, age: Int):
      def combine(other: Person): Person =
        Person(s"${this.name} & ${other.name}", this.age + other.age)

    val person1 = Person("Alice", 30)
    val person2 = Person("Bob", 25)
    val result = RestrictedDynamicMacros.LinearFn.apply((person1, person2))(refs =>
      (refs._1.combine(refs._2), refs._2)
    )
    assertEquals(result, (Person("Alice & Bob", 55), person2))
  }


  test("Combine tracks linearity") {
    val obtained = compileErrors(
      """
      case class Person(name: String, age: Int):
        def combine(other: Person): Person =
          Person(s"${this.name} & ${other.name}", this.age + other.age)

      extension [D <: Tuple, C <: Tuple](p: RestrictedDynamicMacros.Restricted[Person, D, C])
        def combine[D2 <: Tuple, C2 <: Tuple](other: RestrictedDynamicMacros.Restricted[Person, D2, C2]): RestrictedDynamicMacros.Restricted[Person, Tuple.Concat[D, D2], EmptyTuple] =
          p.stageCall[Tuple.Concat[D, D2], EmptyTuple]("combine", Tuple1(other))

      val person1 = Person("Alice", 30)
      val person2 = Person("Bob", 25)
      RestrictedDynamicMacros.LinearFn.apply((person1, person2))(refs =>
        val combined = refs._1.combine(refs._1)
        (combined, refs._2)
      )
    """)
    assert(obtained.contains(TestUtils.affineMsg), s"obtained: $obtained")
  }

  test("Wrong # of arguments fails") {
    val obtained = compileErrors(
      """
      case class Person(name: String, age: Int):
        def combine(other: Person): Person =
          Person(s"${this.name} & ${other.name}", this.age + other.age)

      extension [D <: Tuple, C <: Tuple](p: RestrictedDynamicMacros.Restricted[Person, D, C])
        def combine[D2 <: Tuple, C2 <: Tuple](other: RestrictedDynamicMacros.Restricted[Person, D2, C2]): RestrictedDynamicMacros.Restricted[Person, Tuple.Concat[D, D2], EmptyTuple] =
          p.stageCall[Tuple.Concat[D, D2], EmptyTuple]("combine", Tuple1(other))

      val person1 = Person("Alice", 30)
      val person2 = Person("Bob", 25)
      RestrictedDynamicMacros.LinearFn.apply((person1, person2))(refs =>
        val combined = refs._1.combine(refs._1)
        Tuple1(combined)
      )
    """)
    assert(obtained.contains(TestUtils.argsMsg), s"obtained: $obtained")
  }
