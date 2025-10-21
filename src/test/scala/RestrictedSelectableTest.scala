package linearfn

import munit.FunSuite

class RestrictedSelectableTest extends FunSuite:

  // Note that this works only with product types.
  case class Person(name: String, age: Int)

  test("single argument - return unchanged") {
    val person = Person("Alice", 30)

    val result = RestrictedSelectable.LinearFn.apply(Tuple1(person))(refs =>
      Tuple1(refs._1)
    )

    assertEquals(result, Tuple1(person))
  }

  test("two arguments - return unchanged") {
    val person1 = Person("Alice", 30)
    val person2 = Person("Bob", 25)

    val result = RestrictedSelectable.LinearFn.apply((person1, person2))(refs =>
      (refs._1, refs._2)
    )

    assertEquals(result, (person1, person2))
  }

  test("two arguments - switch order") {
    val person1 = Person("Alice", 30)
    val person2 = Person("Bob", 25)

    // Switching order is allowed - linearity just ensures each arg used exactly once
    val result = RestrictedSelectable.LinearFn.apply((person1, person2))(refs =>
      (refs._2, refs._1)
    )

    assertEquals(result, (person2, person1))
  }

  test("duplicate argument usage fails compilation") {
    val obtained = compileErrors("""
      case class Person(name: String, age: Int)
      val person1 = Person("Alice", 30)
      val person2 = Person("Bob", 25)
      RestrictedSelectable.LinearFn.apply((person1, person2))(refs =>
        (refs._1, refs._1)
      )
    """)
    assert(obtained.contains(TestUtils.linearMsg), s"obtained: $obtained")
  }

  test("manual deps") {
    val str = "hello"
    val num = 42
    val actual = 5

    val result = RestrictedSelectable.LinearFn.apply((str, num))(refs =>
      val tmp: RestrictedSelectable.Restricted[Int, (0, 1)] = RestrictedSelectable.Restricted.LinearRef[Int, (0, 1)](() => actual)
      (tmp, refs._1)
    )

    assertEquals(result, (actual, str))

  }
  test("manual deps 2") {
    val str = "hello"
    val num = 42
    val actual = 5

    val result = RestrictedSelectable.LinearFn.apply((str, num))(refs =>
      val tmp: RestrictedSelectable.Restricted[Int, Tuple1[1]] = RestrictedSelectable.Restricted.LinearRef[Int, Tuple1[1]](() => actual)
      (tmp, refs._1)
    )

    assertEquals(result, (actual, str))

  }
  test("manual non-linear") {
    val obtained = compileErrors(
      """
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
  test("manual non-affine") {
    val obtained = compileErrors(
      """
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

  test("valid field access doesn't throw") {
    val person = Person("Alice", 30)

    val result = RestrictedSelectable.LinearFn.apply(Tuple1(person))(refs =>
      val age = refs._1.age
      Tuple1(age)
    )

    // The result should be the executed value
    assertEquals(result, Tuple1(30))
  }

  test("invalid field access does throw") {
    val obtained = compileErrors("""
      val person = Person("Alice", 30)

      val result = RestrictedSelectable.LinearFn.apply(Tuple1(person))(refs =>
        val age = refs._1.ag2e
        Tuple1(age)
      )
      """)
    assert(obtained.contains(s"ag2e is not a member"), s"obtained: $obtained")
  }

  test("dependencies are tracked for applyDynamic") {
    import RestrictedSelectable.~
    case class Person2(name: String, age: Int):
      def greet(): String = s"Hello, I'm $name"

      def getAge(): Int = age

      def combine(other: Person2): Person2 =
        Person2(s"${this.name} & ${other.name}", this.age + other.age)

    // Force the user to declare methods they are going to use - not very nice.
    extension [D <: Tuple](p: Person2 ~ D)
      def greet(): String ~ D = p.stageCall[String, D]("greet", EmptyTuple)
      def getAge(): Int ~ D = p.stageCall[Int, D]("getAge", EmptyTuple)
      def combine[D2 <: Tuple](other: Person2 ~ D2): Person2 ~ (Tuple.Concat[D, D2]) =
        p.stageCall[Person2, Tuple.Concat[D, D2]]("combine", Tuple1(other))


    val person1 = Person2("Alice", 30)
    val person2 = Person2("Bob", 25)
    val ret = RestrictedSelectable.LinearFn.apply((person1, person2))(refs =>
      val age1 = refs._1.combine(refs._2)
      (age1, refs._2)
    )
    assertEquals(ret, (Person2("Alice & Bob", 55), person2))
  }
  test("dependencies are tracked for applyDynamic and fails") {
    val obtained = compileErrors(
      """
        import RestrictedSelectable.~
        case class Person2(name: String, age: Int):
          def greet(): String = s"Hello, I'm $name"

          def getAge(): Int = age

          def combine(other: Person2): Person2 =
            Person2(s"${this.name} & ${other.name}", this.age + other.age)

        // Force the user to declare methods they are going to use - not very nice.
        extension [D <: Tuple](p: Person2 ~ D)
          def greet(): String ~ D = p.stageCall[String, D]("greet", EmptyTuple)
          def getAge(): Int ~ D = p.stageCall[Int, D]("getAge", EmptyTuple)
          def combine[D2 <: Tuple](other: Person2 ~ D2): Person2 ~ (Tuple.Concat[D, D2]) =
            p.stageCall[Person2, Tuple.Concat[D, D2]]("combine", Tuple1(other))


        val person1 = Person2("Alice", 30)
        val person2 = Person2("Bob", 25)
        val ret = RestrictedSelectable.LinearFn.apply((person1, person2))(refs =>
          val age1 = refs._1.combine(refs._1)
          (age1, refs._2)
        )
      """)
      assert(obtained.contains(TestUtils.affineMsg), s"obtained: $obtained")
  }
