package test.casestudies

import munit.FunSuite
import linearfn.{RestrictedSelectable, VerticalConstraint, HorizontalConstraint}
import scala.annotation.experimental
import test.TestUtils

class RecursiveQueryTest extends FunSuite:

  import RQQueryOps.*

  test("Construct normal query") {
    val q1 = RQQuery[Int]()
    val q2 = RQQuery[Int]()
    for
      a1 <- q1
      a2 <- q2
    yield a1 + a2
  }

  test("Construct normal query base filter") {
    val q1 = RQQuery[Int]()
    val q2 = RQQuery[Int]().union(q1)
    for
      a1 <- q1
      a2 <- q2
    yield a1 + a2
  }

  test("Linear recursive query") {
    val q1 = RQQuery[Int]()
    val q2 = RQQuery[Int]()
    val r = RQQuery.fix(q1, q2)((a1, a2) =>
      (a1, a2)
    )
    println(r)
  }
  test("Linear recursive query with outside ref map/flatMap/filter") {
    val q1 = RQQuery[Int]()
    val q2 = RQQuery[Int]()
    RQQuery.fix(q1, q2)((a1, a2) =>
      val r1 = for
        e1 <- a1
        e2 <- q2 // non-recursive reference
      yield e1 + e2

      val r2 = for
          e1 <- a2
          e2 <- q1 // non-recursive reference
      yield e1 + e2

      (r1, r2)
    )
  }

  test("Linear recursive query with flatMap using comprehension") {
    val q1 = RQQuery[Int]()
    val q2 = RQQuery[Int]()
    RQQuery.fix(q1, q2)((a1, a2) =>
      val r1 = for
        e1 <- a1
        e2 <- a2
      yield e1 + e2

      val r2 = for
        e1 <- a2
        e2 <- a1
      yield e1 + e2

      (r1, r2)
    )
  }

  test("Linear recursive query with flatMap using flatMap") {
    val q1 = RQQuery[Int]()
    val q2 = RQQuery[Int]()
    RQQuery.fix(q1, q2)((a1, a2) =>
      val r1 = a1.flatMap(e1 =>
        a2.map(e2 =>
          e1 + e2
        )
      )
      val r2 = a2.flatMap(e1 =>
        a1.map(e2 =>
          e1 + e2
        )
      )
      (r1, r2)
    )
  }


  test("Linear recursive query with outside ref union") {
    val q1 = RQQuery[Int]()
    val q2 = RQQuery[Int]()
    RQQuery.fix(q1, q2)((a1, a2) =>
      (a1.union(q1), a2)
    )
  }

  test("Linear recursive query with outside ref unionAll") {
    val q1 = RQQuery[Int]()
    val q2 = RQQuery[Int]()
    RQQuery.fix(q1, q2)((a1, a2) =>
      (a1.unionAll(q1), a2)
    )
  }

  test("Non-linear recursive query") {
    val obtained = compileErrors("""
      val q1 = RQQuery[Int]()
      val q2 = RQQuery[Int]()
      RQQuery.fix(q1, q2)((a1, a2) =>
        (a1, a1)
      )
    """)
    assert(obtained.contains(TestUtils.horizontalRelevanceFailed), s"obtained: $obtained")
  }

  test("Non-affine recursive query") {
    val obtained = compileErrors("""
      val q1 = RQQuery[Int]()
      val q2 = RQQuery[Int]()
      RQQuery.fix(q1, q2)((a1, a2) =>
        (a1.union(a1), a2)
      )
    """)
    assert(obtained.contains(TestUtils.horizontalAffineFailed), s"obtained: $obtained")
  }

  test("Non-affine recursive query with unionAll") {
    val obtained = compileErrors("""
      val q1 = RQQuery[Int]()
      val q2 = RQQuery[Int]()
      RQQuery.fix(q1, q2)((a1, a2) =>
        (a1.unionAll(a1), a2)
      )
    """)
    assert(obtained.contains(TestUtils.horizontalAffineFailed), s"obtained: $obtained")
  }

  test("Non-Linear recursive query with map/flatMap/filter") {
    val obtained = compileErrors("""
      val q1 = RQQuery[Int]()
      val q2 = RQQuery[Int]()
      RQQuery.fix(q1, q2)((a1, a2) =>
        val r1 = for
          e1 <- a1
          e2 <- q2 // non-recursive reference
        yield e1 + e2
        (r1, r1)
      )
    """)
    assert(obtained.contains(TestUtils.horizontalRelevanceFailed), s"obtained: $obtained")
  }

  test("Non-affine recursive query with map/flatMap/filter") {
    val obtained = compileErrors("""
      val q1 = RQQuery[Int]()
      val q2 = RQQuery[Int]()
      RQQuery.fix(q1, q2)((a1, a2) =>
        val r1 = for
          e1 <- a1
          e2 <- a1
        yield e1 + e2

        val r2 = for
          e1 <- a2
          e2 <- q1 // non-recursive reference
        yield e1 + e2

        (r1, r2)
      )
    """)
    assert(obtained.contains(TestUtils.horizontalAffineFailed), s"obtained: $obtained")
  }

// SIMPLEFIX (Ergonomic API)

  test("SimpleFix: Linear recursive query") {
    val q1 = RQQuery[Int]()
    val q2 = RQQuery[Int]()
    RQQuery.simpleFix(q1, q2)((a1, a2) => (a1, a2))
  }

  test("SimpleFix: Non-linear recursive query") {
    val obtained = compileErrors(
      """
      val q1 = RQQuery[Int]()
      val q2 = RQQuery[Int]()
      RQQuery.simpleFix(q1, q2)((a1, a2) =>
        (a1, a1)
      )
    """)
    assert(obtained.contains(TestUtils.substructuralConstraintFailed), s"obtained: $obtained")
  }

  test("SimpleFix: Wrong number of arguments") {
    val obtained = compileErrors(
      """
      val q1 = RQQuery[Int]()
      val q2 = RQQuery[Int]()
      RQQuery.simpleFix(q1, q2)((a1, a2) =>
        (a1, a2, a1)
      )
    """)
    assert(obtained.contains("simpleFix requires same number of args and returns"), s"obtained: $obtained")
  }

// CUSTOMFIX


    test("CustomFix: Linear recursive query") {
      val q1 = RQQuery[Int]()
      val q2 = RQQuery[Int]()
      RQQuery.customFix(q1, q2)((a1, a2) => (a1, a2))
    }
    test("CustomFix: Linear recursive query with outside ref map/flatMap/filter") {
      val q1 = RQQuery[Int]()
      val q2 = RQQuery[Int]()
      RQQuery.customFix(q1, q2)((a1, a2) =>
        val r1 = for
          e1 <- a1
          e2 <- q2 // non-recursive reference
        yield e1 + e2

        val r2 = for
          e1 <- a2
          e2 <- q1 // non-recursive reference
        yield e1 + e2

        (r1, r2)
      )
    }

    test("CustomFix: Linear recursive query with flatMap using comprehension") {
      val q1 = RQQuery[Int]()
      val q2 = RQQuery[Int]()
      RQQuery.customFix(q1, q2)((a1, a2) =>
        val r1 = for
          e1 <- a1
          e2 <- a2
        yield e1 + e2

        val r2 = for
          e1 <- a2
          e2 <- a1
        yield e1 + e2

        (r1, r2)
      )
    }

    test("CustomFix: Linear recursive query with flatMap using flatMap") {
      val q1 = RQQuery[Int]()
      val q2 = RQQuery[Int]()
      RQQuery.customFix(q1, q2)((a1, a2) =>
        val r1 = a1.flatMap(e1 =>
          a2.map(e2 =>
            e1 + e2
          )
        )
        val r2 = a2.flatMap(e1 =>
          a1.map(e2 =>
            e1 + e2
          )
        )
        (r1, r2)
      )
    }


    test("CustomFix: Linear recursive query with outside ref union") {
      val q1 = RQQuery[Int]()
      val q2 = RQQuery[Int]()
      RQQuery.customFix(q1, q2)((a1, a2) =>
        (a1.union(q1), a2)
      )
    }

    test("CustomFix: Linear recursive query with outside ref unionAll") {
      val q1 = RQQuery[Int]()
      val q2 = RQQuery[Int]()
      RQQuery.customFix(q1, q2)((a1, a2) =>
        (a1.unionAll(q1), a2)
      )
    }

    test("CustomFix: Non-linear recursive query") {
      val obtained = compileErrors(
        """
        val q1 = RQQuery[Int]()
        val q2 = RQQuery[Int]()
        RQQuery.customFix(q1, q2)((a1, a2) =>
          (a1, a1)
        )
      """)
      assert(obtained.contains(TestUtils.substructuralConstraintFailed), s"obtained: $obtained")
    }

    test("CustomFix: Non-affine recursive query") {
      val obtained = compileErrors(
        """
        val q1 = RQQuery[Int]()
        val q2 = RQQuery[Int]()
        RQQuery.customFix(q1, q2)((a1, a2) =>
          (a1.union(a1), a2)
        )
      """)
      assert(obtained.contains(TestUtils.substructuralConstraintFailed), s"obtained: $obtained")
    }

    test("CustomFix: Non-affine recursive query with unionAll") {
      val obtained = compileErrors(
        """
        val q1 = RQQuery[Int]()
        val q2 = RQQuery[Int]()
        RQQuery.customFix(q1, q2)((a1, a2) =>
          (a1.unionAll(a1), a2)
        )
      """)
      assert(obtained.contains(TestUtils.substructuralConstraintFailed), s"obtained: $obtained")
    }

    test("CustomFix: Non-Linear recursive query with map/flatMap/filter") {
      val obtained = compileErrors(
        """
        val q1 = RQQuery[Int]()
        val q2 = RQQuery[Int]()
        RQQuery.customFix(q1, q2)((a1, a2) =>
          val r1 = for
            e1 <- a1
            e2 <- q2 // non-recursive reference
          yield e1 + e2
          (r1, r1)
        )
      """)
      assert(obtained.contains(TestUtils.substructuralConstraintFailed), s"obtained: $obtained")
    }

    test("CustomFix: Non-affine recursive query with map/flatMap/filter") {
      val obtained = compileErrors(
        """
        val q1 = RQQuery[Int]()
        val q2 = RQQuery[Int]()
        RQQuery.customFix(q1, q2)((a1, a2) =>
          val r1 = for
            e1 <- a1
            e2 <- a1
          yield e1 + e2

          val r2 = for
            e1 <- a2
            e2 <- q1 // non-recursive reference
          yield e1 + e2

          (r1, r2)
        )
      """)
      assert(obtained.contains(TestUtils.substructuralConstraintFailed), s"obtained: $obtained")
    }

// ADDITIONAL CONSTRAINT TESTS

  test("CustomFix: Wrong number of arguments (more returns than args)") {
    val obtained = compileErrors(
      """
      val q1 = RQQuery[Int]()
      val q2 = RQQuery[Int]()
      RQQuery.customFix(q1, q2)((a1, a2) =>
        (a1, a2, a1)  // 3 returns but only 2 arguments
      )
    """)
    assert(obtained.contains("customFix requires same number of args and returns"), s"obtained: $obtained")
  }

  test("CustomFix: Wrong number of arguments (fewer returns than args)") {
    // When tuple sizes don't match, the base linearity constraints fail first
    // because the type alignment is incorrect
    val obtained = compileErrors(
      """
      val q1 = RQQuery[Int]()
      val q2 = RQQuery[Int]()
      val q3 = RQQuery[Int]()
      RQQuery.customFix(q1, q2, q3)((a1, a2, a3) =>
        (a1, a2)  // 2 returns but 3 arguments
      )
    """)
    // The base constraints fail before we get to the tuple size check
    assert(obtained.contains("Substructural constraint not satisfied"), s"obtained: $obtained")
  }

  test("CustomFix: Non-Query type argument fails base constraints") {
    // When a non-Query type is passed, the base linearity constraints fail
    // because the argument type doesn't match RQQuery[?]
    val obtained = compileErrors(
      """
      val q1 = RQQuery[Int]()
      val nonQuery = 42
      RQQuery.customFix(q1, nonQuery)((a1, a2) =>
        (a1, a1)
      )
    """)
    // The error comes from the substructural constraint check, not the RQQuery type check
    // This is because type inference tries to work with (Query[Int], Int)
    assert(obtained.contains("Substructural constraint not satisfied"), s"obtained: $obtained")
  }

