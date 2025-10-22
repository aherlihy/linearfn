package linearfn

/**
 * Tests for RestrictedSelectable implementation.
 */
class RestrictedSelectableTest extends LinearFnTestSuite(RestrictedSelectable, "RestrictedSelectable"):

  // Compile-time error tests (must use literal strings with compileErrors)
  test("RestrictedSelectable: duplicate argument usage fails compilation") {
    val obtained = compileErrors("""
      val str = "hello"
      val num = 42
      RestrictedSelectable.LinearFn.apply((str, num))(refs =>
        (refs._1, refs._1)
      )
    """)
    assert(obtained.contains(TestUtils.linearMsg), s"obtained: $obtained")
  }

  test("RestrictedSelectable: manual non-linear") {
    val obtained = compileErrors("""
      val str = "hello"
      val num = 42
      val actual = 5
      val result = RestrictedSelectable.LinearFn.apply((str, num))(refs =>
        val tmp: RestrictedSelectable.Restricted[Int, (0, 0)] = RestrictedSelectable.Restricted.LinearRef[Int, (0, 0)](() => actual)
        (tmp, refs._1)
      )
    """)
    assert(obtained.contains(TestUtils.affineMsg), s"obtained: $obtained")
  }

  test("RestrictedSelectable: manual non-affine") {
    val obtained = compileErrors("""
      val str = "hello"
      val num = 42
      val actual = 5
      val result = RestrictedSelectable.LinearFn.apply((str, num))(refs =>
        val tmp: RestrictedSelectable.Restricted[Int, Tuple1[0]] = RestrictedSelectable.Restricted.LinearRef[Int, Tuple1[0]](() => actual)
        (tmp, refs._1)
      )
    """)
    assert(obtained.contains(TestUtils.linearMsg), s"obtained: $obtained")
  }

  // Note: RestrictedSelectable doesn't support primitive operators since it uses Selectable, not Dynamic
  // test("RestrictedSelectable: dependencies are tracked for primitive operator") {
  //   val obtained = compileErrors("""
  //     val str = "hello"
  //     val num = 42
  //     RestrictedSelectable.LinearFn.apply((str, num))(refs => (refs._2 + refs._2, refs._1))
  //   """)
  //   assert(obtained.contains(TestUtils.affineMsg), s"obtained: $obtained")
  // }

  // Runtime tests for field and method access
  test("valid field access on case class") {
    case class Person(name: String, age: Int)
    val person = Person("Alice", 30)
    // Field access works - just verify no runtime errors
    RestrictedSelectable.LinearFn.apply(Tuple1(person))(refs =>
      val _age = refs._1.age  // This should compile and run without errors
      Tuple1(refs._1)
    )
    assert(true) // If we get here, field access worked
  }

  test("field access fails for non-existing field") {
    val obtained = compileErrors("""
      case class Person(name: String, age: Int)
      val person = Person("Alice", 30)
      RestrictedSelectable.LinearFn.apply(Tuple1(person))(refs =>
        val x = refs._1.nonExistentField
        Tuple1(refs._1)
      )
    """)
    assert(obtained.contains("value nonExistentField is not a member"), s"obtained: $obtained")
  }

  // Note: RestrictedSelectable doesn't support field access on primitive types (uses Selectable, not Dynamic)
  // test("valid field access on primitive type") {
  //   val str = "hello"
  //   val result = RestrictedSelectable.LinearFn.apply(Tuple1(str))(refs =>
  //     val len = refs._1.length
  //     Tuple1(len)
  //   )
  //   assertEquals(result, Tuple1(5))
  // }

  test("dependencies tracked for combine method") {
    case class Person(name: String, age: Int):
      def combine(other: Person): Person =
        Person(s"${this.name} & ${other.name}", this.age + other.age)

    extension [D <: Tuple](p: RestrictedSelectable.Restricted[Person, D])
      def combine[D2 <: Tuple](other: RestrictedSelectable.Restricted[Person, D2]): RestrictedSelectable.Restricted[Person, Tuple.Concat[D, D2]] =
        p.stageCall[Person, Tuple.Concat[D, D2]]("combine", Tuple1(other))

    val person1 = Person("Alice", 30)
    val person2 = Person("Bob", 25)
    val result = RestrictedSelectable.LinearFn.apply((person1, person2))(refs =>
      (refs._1.combine(refs._2), refs._2)
    )
    assertEquals(result, (Person("Alice & Bob", 55), person2))
  }

  test("combine method fails with duplicate use") {
    val obtained = compileErrors("""
      case class Person(name: String, age: Int):
        def combine(other: Person): Person =
          Person(s"${this.name} & ${other.name}", this.age + other.age)

      extension [D <: Tuple](p: RestrictedSelectable.Restricted[Person, D])
        def combine[D2 <: Tuple](other: RestrictedSelectable.Restricted[Person, D2]): RestrictedSelectable.Restricted[Person, Tuple.Concat[D, D2]] =
          p.stageCall[Person, Tuple.Concat[D, D2]]("combine", Tuple1(other))

      val person1 = Person("Alice", 30)
      val person2 = Person("Bob", 25)
      RestrictedSelectable.LinearFn.apply((person1, person2))(refs =>
        val combined = refs._1.combine(refs._1)
        Tuple1(combined)
      )
    """)
    assert(obtained.contains("Number of actual arguments must match") || obtained.contains(TestUtils.affineMsg), s"obtained: $obtained")
  }
