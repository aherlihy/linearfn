package test

import munit.FunSuite
import linearfn.RestrictedSelectable
import scala.annotation.experimental

/**
 * Tests for CSQueryBuilder case study: Builder Pattern
 *
 * Demonstrates: Builder pattern with build() consuming the builder
 * Key: Focus on linearity constraints, not query complexity
 */
@experimental
class CSQueryBuilderTest extends FunSuite:
  import TestUtils.*

  test("Builder pattern: build consumes builder (applyConsumed)") {
    import CSQueryBuilderOps.*

    val builder = CSQueryBuilder(Nil)

    // applyConsumed: ensures build() is called
    val result = RestrictedSelectable.LinearFn.applyConsumed(Tuple1(builder))(refs =>
      val query = refs._1.select("name").from("users").build()  // @consumed
      Tuple1(query)
    )

    assertEquals(result._1, "SELECT name FROM users")
  }

  test("NEGATIVE: builder must be built when using applyConsumed") {
    import CSQueryBuilderOps.*

    val obtained = compileErrors("""
      import CSQueryBuilderOps.*
      import linearfn.RestrictedSelectable

      val builder = CSQueryBuilder(Nil)

      RestrictedSelectable.LinearFn.applyConsumed(Tuple1(builder))(refs =>
        val partial = refs._1.select("id").from("users")
        Tuple1(partial)  // Error: must call build() to consume
      )
    """)

    assert(obtained.contains(consumptionExactlyOneMsg), s"Expected consumption error but got: $obtained")
  }

  test("NEGATIVE: cannot use builder after build") {
    import CSQueryBuilderOps.*

    val obtained = compileErrors("""
      import CSQueryBuilderOps.*
      import linearfn.RestrictedSelectable

      val builder = CSQueryBuilder(Nil)

      RestrictedSelectable.LinearFn.apply(Tuple1(builder))(refs =>
        val query = refs._1.select("*").build().from("users")  // Error: from after @consumed build
        Tuple1(query)
      )
    """)

    assert(obtained.contains(argsMsg), s"Expected args error but got: $obtained")
  }
