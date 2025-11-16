package test.casestudies

import linearfn.{HorizontalConstraint, RestrictedSelectable, VerticalConstraint}
import munit.FunSuite
import test.TestUtils
import scala.NamedTuple.*

import scala.annotation.experimental

type IntRow = (i1: Int, i2: Int)

class LinearDatalogTest extends FunSuite:
  import QueryOps.*

  test("Construct normal query") {
    val q1 = Query.edb[IntRow]("q1")
    val q2 = Query.edb[IntRow]("q2")
    val query1 = for
      a1 <- q1
      a2 <- q2
    yield a1.i1 + a2.i2

    val query2 = q1.flatMap(a1 =>
      q2.map(a2 =>
        a1.i1 + a2.i1
      )
    )

    println(query2)
  }

  test("Linear recursive query") {
    val q1 = Query.edb[IntRow]("q1")
    val q2 = Query.edb[IntRow]("q2")
    val q3 = Query.edb[IntRow]("q3")
    val r = Query.fixedPoint(q1, q2.union(q3))((a1, a2) =>
      val r1 = a1.union(q3)
      val r2 = a2.map(e => (i1 = e.i1, i2 = e.i2))
      (r1, r2)
    )
    println(r)
  }
