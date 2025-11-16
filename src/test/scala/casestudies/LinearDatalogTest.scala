package casestudies

import linearfn.{HorizontalConstraint, RestrictedSelectable, VerticalConstraint}
import munit.FunSuite
import test.TestUtils
import scala.NamedTuple.*

import scala.annotation.experimental

type IntRow = (i1: Int, i2: Int)

class LinearDatalogTest extends FunSuite:

  test("Construct normal query") {
    val q1 = Query.EDB[IntRow]("q1")
    val q2 = Query.EDB[IntRow]("q2")
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
    val q1 = Query[Int]()
    val q2 = Query[Int]()
    val r = Query.fix(q1, q2)((a1, a2) =>
      (a1, a2)
    )
    println(r)
  }
