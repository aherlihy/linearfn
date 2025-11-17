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

  test("Generate Datalog from simple query") {
    val edges = Query.edb[IntRow]("edges")
    val datalog = edges.peek()
    // EDB queries now generate rules
    val expected = "p0(v0, v1) :- edges(v0, v1)."
    assertEquals(datalog, expected)
  }

  test("Generate Datalog from union query") {
    val q1 = Query.edb[IntRow]("q1")
    val q2 = Query.edb[IntRow]("q2")
    val unionQuery = q1.union(q2)
    val datalog = unionQuery.peek()
    // Union generates intermediate predicates for each branch, then combines them
    val expected = """p2(v0, v1) :- q1(v0, v1).

p3(v2, v3) :- q2(v2, v3).

p1(v4) :- p2(v4).
p1(v4) :- p3(v4)."""
    assertEquals(datalog, expected)
  }

  test("Generate Datalog from map query") {
    val edges = Query.edb[IntRow]("edges")
    val mapped = edges.map(e => (i1 = e.i2, i2 = e.i1))
    val datalog = mapped.peek()
    // Should generate: intermediate predicate for edges, then map it
    val expected = """p5(v0, v1) :- edges(v0, v1).

p4(v3, v2) :- p5(v2, v3)."""
    assertEquals(datalog, expected)
  }
  
  test("Generate Datalog from constant map query") {
    val edges = Query.edb[IntRow]("edges")
    val mapped = edges.map(e => (i1 = e.i2, i2 = Expr.ExprLit(10)))
    val datalog = mapped.peek()
    // Should generate: intermediate predicate for edges, then map with constant
    val expected = """p7(v0, v1) :- edges(v0, v1).

p6(v3, 10) :- p7(v2, v3)."""
    assertEquals(datalog, expected)
  }

  test("Generate Datalog from identity map") {
    val edges = Query.edb[IntRow]("edges")
    val mapped = edges.map(e => (i1 = e.i1, i2 = e.i2))
    val datalog = mapped.peek()
    // Should generate: intermediate predicate for edges, then identity map
    val expected = """p9(v0, v1) :- edges(v0, v1).

p8(v2, v3) :- p9(v2, v3)."""
    assertEquals(datalog, expected)
  }

  test("Generate Datalog from complex query") {
    // More complex: union then map
    val q1 = Query.edb[IntRow]("q1")
    val q2 = Query.edb[IntRow]("q2")
    val unionQuery = q1.union(q2)
    val mapped = unionQuery.map(e => (i1 = e.i2, i2 = e.i1))
    val datalog = mapped.peek()
    // Union creates intermediate predicates, then map swaps fields
    val expected = """p12(v0, v1) :- q1(v0, v1).

p13(v2, v3) :- q2(v2, v3).

p11(v4) :- p12(v4).
p11(v4) :- p13(v4).

p10(v6, v5) :- p11(v5, v6)."""
    assertEquals(datalog, expected)
  }

  test("Generate Datalog from constant filter query 1") {
    val edges = Query.edb[IntRow]("edges")
    val mapped = edges.filter(e => e.i2 == Expr.ExprLit(10))
    val datalog = mapped.peek()
    // Should generate: intermediate predicate for edges, then filter with constant
    val expected = """p15(v0, v1) :- edges(v0, v1).

p14(v2, v3) :- p15(v2, v3), v3 == 10."""
    assertEquals(datalog, expected)
  }
  test("Generate Datalog from constant filter query 2") {
    val edges = Query.edb[IntRow]("edges")
    val mapped = edges.filter(e => (e.i2 == Expr.ExprLit(10)) && (e.i1 == Expr.ExprLit(5)))
    val datalog = mapped.peek()
    // Should generate: intermediate predicate for edges, then filter with two constants
    val expected = """p17(v0, v1) :- edges(v0, v1).

p16(v2, v3) :- p17(v2, v3), v3 == 10, v2 == 5."""
    assertEquals(datalog, expected)
  }

  test("Generate Datalog from simple flatMap join") {
    val path = Query.edb[IntRow]("path")
    val query = path.flatMap(p1 =>
      path.filter(p2 => p1.i2 == p2.i1)
          .map(p2 => (i1 = p1.i1, i2 = p2.i2))
    )
    val datalog = query.peek()
    // Should generate: result(x, y) :- path(x, z), path(z, y)
    // Where p1.i2 == p2.i1 becomes the join condition (second field of first == first field of second)
    // And output is (p1.i1, p2.i2) which is (first of first, second of second)
    // Variables are unified inline: v6 is replaced with v3
    val expected = """p19(v0, v1) :- path(v0, v1).

p20(v4, v5) :- path(v4, v5).

p18(v2, v7) :- p19(v2, v3), p20(v3, v7)."""
    assertEquals(datalog, expected)
  }

  test("Generate Datalog from flatMap with two different EDBs") {
    val edges = Query.edb[IntRow]("edges")
    val nodes = Query.edb[IntRow]("nodes")
    val query = edges.flatMap(e =>
      nodes.filter(n => e.i2 == n.i1)
           .map(n => (i1 = e.i1, i2 = n.i2))
    )
    val datalog = query.peek()
    // Should generate: result(x, y) :- edges(x, z), nodes(z, y)
    // Variables are unified inline: v6 is replaced with v3
    val expected = """p22(v0, v1) :- edges(v0, v1).

p23(v4, v5) :- nodes(v4, v5).

p21(v2, v7) :- p22(v2, v3), p23(v3, v7)."""
    assertEquals(datalog, expected)
  }

  test("Generate Datalog from flatMap with join and constant constraint") {
    val edges = Query.edb[IntRow]("edges")
    val nodes = Query.edb[IntRow]("nodes")
    val query = edges.flatMap(e =>
      nodes.filter(n => (e.i2 == n.i1) && (n.i2 == Expr.ExprLit(10)))
           .map(n => (i1 = e.i1, i2 = n.i2))
    )
    val datalog = query.peek()
    // Should generate: result(x, y) :- edges(x, z), nodes(z, y), y == 10
    // Variables unified inline (e.i2 == n.i1 becomes shared variable)
    // Constant constraint kept (n.i2 == 10)
    val expected = """p25(v0, v1) :- edges(v0, v1).

p26(v4, v5) :- nodes(v4, v5).

p24(v2, v7) :- p25(v2, v3), p26(v3, v7), v7 == 10."""
    assertEquals(datalog, expected)
  }
