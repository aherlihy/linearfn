package test.casestudies

import munit.FunSuite
import linearfn.{RestrictedSelectable}
import scala.annotation.experimental
import test.TestUtils

class RecursiveQueryTest extends FunSuite:

  import QueryOps.*

  test("Construct normal query") {
    val q1 = Query[Int]()
    val q2 = Query[Int]()
    for
      a1 <- q1
      a2 <- q2
    yield a1 + a2
  }

  test("Linear recursive query") {
    val q1 = Query[Int]()
    val q2 = Query[Int]()
    Query.fix(q1, q2)((a1, a2) =>
      (a1, a2)
    )
  }
  test("Linear recursive query with outside ref map/flatMap/filter") {
    val q1 = Query[Int]()
    val q2 = Query[Int]()
    Query.fix(q1, q2)((a1, a2) =>
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
    val q1 = Query[Int]()
    val q2 = Query[Int]()
    Query.fix(q1, q2)((a1, a2) =>
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
    val q1 = Query[Int]()
    val q2 = Query[Int]()
    Query.fix(q1, q2)((a1, a2) =>
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
    val q1 = Query[Int]()
    val q2 = Query[Int]()
    Query.fix(q1, q2)((a1, a2) =>
      (a1.union(q1), a2)
    )
  }

  test("Linear recursive query with outside ref unionAll") {
    val q1 = Query[Int]()
    val q2 = Query[Int]()
    Query.fix(q1, q2)((a1, a2) =>
      (a1.unionAll(q1), a2)
    )
  }

  test("Non-linear recursive query") {
    val obtained = compileErrors("""
      val q1 = Query[Int]()
      val q2 = Query[Int]()
      Query.fix(q1, q2)((a1, a2) =>
        (a1, a1)
      )
    """)
    assert(obtained.contains(TestUtils.horizontalRelevanceFailed), s"obtained: $obtained")
  }

  test("Non-affine recursive query") {
    val obtained = compileErrors("""
      val q1 = Query[Int]()
      val q2 = Query[Int]()
      Query.fix(q1, q2)((a1, a2) =>
        (a1.union(a1), a2)
      )
    """)
    assert(obtained.contains(TestUtils.horizontalAffineFailed), s"obtained: $obtained")
  }

  test("Non-affine recursive query with unionAll") {
    val obtained = compileErrors("""
      val q1 = Query[Int]()
      val q2 = Query[Int]()
      Query.fix(q1, q2)((a1, a2) =>
        (a1.unionAll(a1), a2)
      )
    """)
    assert(obtained.contains(TestUtils.horizontalAffineFailed), s"obtained: $obtained")
  }

  test("Non-Linear recursive query with map/flatMap/filter") {
    val obtained = compileErrors("""
      val q1 = Query[Int]()
      val q2 = Query[Int]()
      Query.fix(q1, q2)((a1, a2) =>
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
      val q1 = Query[Int]()
      val q2 = Query[Int]()
      Query.fix(q1, q2)((a1, a2) =>
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

