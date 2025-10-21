package linearfn

import munit.FunSuite

class RestrictedDynamicMacrosTest extends FunSuite:

  test("single argument - return unchanged") {
    val str = "hello"

    val result = RestrictedDynamicMacros.LinearFn.apply(Tuple1(str))(refs =>
      Tuple1(refs._1)
    )

    assertEquals(result, Tuple1(str))
  }

  test("two arguments - return unchanged") {
    val str = "hello"
    val num = 42

    val result = RestrictedDynamicMacros.LinearFn.apply((str, num))(refs =>
      (refs._1, refs._2)
    )

    assertEquals(result, (str, num))
  }

  test("two arguments - switch order") {
    val str = "hello"
    val num = 42

    // Switching order is allowed - linearity just ensures each arg used exactly once
    val result = RestrictedDynamicMacros.LinearFn.apply((str, num))(refs =>
      (refs._2, refs._1)
    )

    assertEquals(result, (num, str))
  }

  test("duplicate argument usage fails compilation") {
    val obtained = compileErrors("""
      val str = "hello"
      val num = 42
      RestrictedDynamicMacros.LinearFn.apply((str, num))(refs =>
        (refs._1, refs._1)
      )
    """)
    assert(obtained.contains(TestUtils.linearMsg))
  }

  test("manual deps") {
    val str = "hello"
    val num = 42
    val actual = 5

    val result = RestrictedDynamicMacros.LinearFn.apply((str, num))(refs =>
      val tmp: RestrictedDynamicMacros.Restricted[Int, (0, 1)] = RestrictedDynamicMacros.Restricted.LinearRef[Int, (0, 1)](() => actual)
      (tmp, refs._1)
    )

    assertEquals(result, (actual, str))

  }
  test("manual deps 2") {
    val str = "hello"
    val num = 42
    val actual = 5

    val result = RestrictedDynamicMacros.LinearFn.apply((str, num))(refs =>
      val tmp: RestrictedDynamicMacros.Restricted[Int, Tuple1[1]] = RestrictedDynamicMacros.Restricted.LinearRef[Int, Tuple1[1]](() => actual)
      (tmp, refs._1)
    )

    assertEquals(result, (actual, str))
  }
  test("manual non-linear") {
    val obtained = compileErrors("""
      val str = "hello"
      val num = 42
      val actual = 5

      val result = RestrictedDynamicMacros.LinearFn.apply((str, num))(refs =>
        val tmp: RestrictedDynamicMacros.Restricted[Int, (0, 0)] = RestrictedDynamicMacros.Restricted.LinearRef[Int, (0, 0)](() => actual)
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

      val result = RestrictedDynamicMacros.LinearFn.apply((str, num))(refs =>
        val tmp: RestrictedDynamicMacros.Restricted[Int, Tuple1[0]] = RestrictedDynamicMacros.Restricted.LinearRef[Int, Tuple1[0]](() => actual)
        (tmp, refs._1)
      )
      """)
    assert(obtained.contains(TestUtils.linearMsg), s"obtained: $obtained")
  }
  test("dependencies are tracked for int +") {
    val str = "hello"
    val num = 42
    val actual = 5

    val obtained = compileErrors("""
      RestrictedDynamicMacros.LinearFn.apply((str, num))(refs =>
        (refs._2 + refs._2, refs._1)
      )
    """)

    assert(obtained.contains(TestUtils.affineMsg), s"obtained: $obtained")
  }

  test("valid field access doesn't throw") {
    val person = Person("Alice", 30)

    val result = RestrictedDynamicMacros.LinearFn.apply(Tuple1(person))(refs =>
      val age = refs._1.age
      Tuple1(age)
    )

    // The result should be the executed value
    assertEquals(result, Tuple1(30))
  }

  test("invalid field access does throw") {
    val obtained = compileErrors(
      """
       val person1 = Person("Alice", 30)
       val person2 = Person("Bob", 10)

       val result = RestrictedDynamicMacros.LinearFn.apply((person1, person2))(refs =>
         val age = refs._1.ag2e
         (age, refs._2)
       )
       """)
    assert(obtained.contains(s"'ag2e' not found"), s"obtained: $obtained")
  }

  test("valid method access doesn't throw") {
    val person = Person("Alice", 30)

    val result = RestrictedDynamicMacros.LinearFn.apply(Tuple1(person))(refs =>
      val age = refs._1.greet()
      Tuple1(age)
    )

    // The result should be the executed value
    assertEquals(result, Tuple1("Hello, I'm Alice"))
  }

  test("invalid method access does throw") {
    val obtained = compileErrors(
      """
       val person1 = Person("Alice", 30)
       val person2 = Person("Bob", 10)

       val result = RestrictedDynamicMacros.LinearFn.apply((person1, person2))(refs =>
         val age = refs._1.greet2
         (age, refs._2)
       )
       """)
    assert(obtained.contains(s"'greet2' not found"), s"obtained: $obtained")
  }