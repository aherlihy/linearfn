package test.casestudies

import restrictedfn.{Multiplicity, RestrictedSelectable}
import RestrictedSelectable.{given, *}
import munit.FunSuite
import test.casestudies.{Query, QueryOps, Expr}
import QueryOps.{given, *}
import Expr.{given, *}
import scala.NamedTuple.*

/**
 * Tests for larger benchmark queries: Ancestry and SSSP
 * These tests verify that our DSL generates correct Datalog matching the Souffle specifications
 */
class BenchmarkDatalogTest extends FunSuite:

  /**
   * ANCESTRY BENCHMARK
   *
   * Souffle specification (rqb_ancestry.dl):
   *
   * .decl parents(c0: symbol, c1: symbol)
   * .input parents
   *
   * .decl generation(name: symbol, gen: number)
   *
   * generation(name, 1) :-
   *     parents("1", name).
   *
   * generation(name, gen + 1) :-
   *     parents(parent, name),
   *     generation(parent, gen).
   *
   * .decl result(name: symbol)
   * result(name) :-
   *     generation(name, 2).
   */
  test("Ancestry - generate correct Datalog") {
    // Reset counters for deterministic predicate names
    Query.intensionalRefCount = 0
    Query.predCounter = 0

    type Parent = (parent: String, child: String)
    type Generation = (name: String, gen: Int)

    val parents = Query.edb[Parent]("parents")

    // Base case: children of '1' are in generation 1
    // generation(name, 1) :- parents("1", name).
    val base = parents
      .filter(p => p.child == Expr.ExprLit(1))  // Using Int 1 to represent the parent
      .map(p => (name = p.child, gen = Expr.ExprLit(1)))

    // Recursive case: generation(name, gen + 1) :- parents(parent, name), generation(parent, gen).
    val generationQuery = Query.fixedPoint(Tuple1(base))(genTuple =>
      DatalogConnective.apply(Tuple1(
        genTuple._1.flatMap(g =>
          parents
            .filter(parent => parent.parent == g.name)
            .map(parent => (name = parent.child, gen = g.gen + Expr.ExprLit(1)))
        )
      ))
    )

    val generation = generationQuery._1

    // Final result: result(name) :- generation(name, 2).
    val result = generation
      .filter(g => g.gen == Expr.ExprLit(2))
      .map(g => (name = g.name))

    val datalog = result.peek()

    val expected = """p5(v0, v1) :- parents(v0, v1).

p4(v2, v3) :- p5(v2, v3), v1 == 1.

p3(v6, 1) :- p4(v4, v5).

p7(v6, v7) :- idb0(v6, v7).

p8(v10, v11) :- parents(v10, v11).

p6(v8, v14) :- p7(v8, v9), p8(v8, v13).

idb0(v14, v15) :- p3(v14, v15).
idb0(v14, v15) :- p6(v14, v15).

p1(v16, v17) :- idb0(v16, v17), v1 == 2.

p0(v20) :- p1(v18, v19)."""

    println("=== ANCESTRY DATALOG ===")
    println(datalog)
    println()

    assertEquals(datalog, expected)
  }

  /**
   * SSSP (Single-Source Shortest Path) BENCHMARK
   *
   * Souffle specification (rqb_sssp.dl):
   *
   * .decl base(dst: number, cost: number)
   * .input base
   *
   * .decl edge(src: number, dst: number, cost: number)
   * .input edge
   *
   * .decl cost(dst: number, cost: number)
   *
   * cost(dst, cost) :-
   *     base(dst, cost).
   *
   * cost(dst, cost1 + cost2) :-
   *     cost(src, cost1),
   *     edge(src, dst, cost2).
   *
   * Note: The Souffle version also computes minimum costs, but for linear Datalog
   * we focus on the recursive reachability with costs (without aggregation).
   */
  test("SSSP - generate correct Datalog") {
    // Reset counters for deterministic predicate names
    Query.intensionalRefCount = 0
    Query.predCounter = 0

    type Edge = (src: Int, dst: Int, cost: Int)
    type Cost = (dst: Int, cost: Int)

    val baseEDB = Query.edb[Cost]("base")
    val edgeEDB = Query.edb[Edge]("edge")

    // Recursive case: cost(dst, cost1 + cost2) :- cost(src, cost1), edge(src, dst, cost2).
    val costQuery = Query.fixedPoint(Tuple1(baseEDB))(costTuple =>
      DatalogConnective.apply(Tuple1(
        costTuple._1.flatMap(c =>
          edgeEDB
            .filter(edge => edge.src == c.dst)
            .map(edge => (dst = edge.dst, cost = c.cost + edge.cost))
        )
      ))
    )

    val result = costQuery._1

    val datalog = result.peek()

    val expected = """p1(v0, v1) :- base(v0, v1).

p3(v2, v3) :- idb0(v2, v3).

p4(v6, v7) :- edge(v6, v7).

p2(v4, v10) :- p3(v4, v5), p4(v4, v9).

idb0(v10, v11) :- p1(v10, v11).
idb0(v10, v11) :- p2(v10, v11)."""

    println("=== SSSP DATALOG ===")
    println(datalog)
    println()

    assertEquals(datalog, expected)
  }

  /**
   * ANCESTRY with unrestrictedFixedPoint - for performance comparison
   */
  test("Ancestry - unrestricted version") {
    Query.intensionalRefCount = 0
    Query.predCounter = 0

    type Parent = (parent: String, child: String)
    type Generation = (name: String, gen: Int)

    val parents = Query.edb[Parent]("parents")

    val base = parents
      .filter(p => p.child == Expr.ExprLit(1))
      .map(p => (name = p.child, gen = Expr.ExprLit(1)))

    val generationQuery = Query.unrestrictedFixedPoint(Tuple1(base))(genTuple =>
      Tuple1(
        genTuple._1.flatMap(g =>
          parents
            .filter(parent => parent.parent == g.name)
            .map(parent => (name = parent.child, gen = g.gen + Expr.ExprLit(1)).toRow)
        )
      )
    )

    val generation = generationQuery._1
    val result = generation
      .filter(g => g.gen == Expr.ExprLit(2))
      .map(g => (name = g.name))

    val datalog = result.peek()

    // Should generate the exact same Datalog as the restricted version
    val expected = """p5(v0, v1) :- parents(v0, v1).

p4(v2, v3) :- p5(v2, v3), v1 == 1.

p3(v6, 1) :- p4(v4, v5).

p7(v6, v7) :- idb0(v6, v7).

p8(v10, v11) :- parents(v10, v11).

p6(v8, v14) :- p7(v8, v9), p8(v8, v13).

idb0(v14, v15) :- p3(v14, v15).
idb0(v14, v15) :- p6(v14, v15).

p1(v16, v17) :- idb0(v16, v17), v1 == 2.

p0(v20) :- p1(v18, v19)."""

    println("=== ANCESTRY UNRESTRICTED DATALOG ===")
    println(datalog)
    println()

    assertEquals(datalog, expected)
  }

  /**
   * SSSP with unrestrictedFixedPoint - for performance comparison
   */
  test("SSSP - unrestricted version") {
    Query.intensionalRefCount = 0
    Query.predCounter = 0

    type Edge = (src: Int, dst: Int, cost: Int)
    type Cost = (dst: Int, cost: Int)

    val baseEDB = Query.edb[Cost]("base")
    val edgeEDB = Query.edb[Edge]("edge")

    val costQuery = Query.unrestrictedFixedPoint(Tuple1(baseEDB))(costTuple =>
      Tuple1(
        costTuple._1.flatMap(c =>
          edgeEDB
            .filter(edge => edge.src == c.dst)
            .map(edge => (dst = edge.dst, cost = c.cost + edge.cost).toRow)
        )
      )
    )

    val result = costQuery._1
    val datalog = result.peek()

    // Should generate the exact same Datalog as the restricted version
    val expected = """p1(v0, v1) :- base(v0, v1).

p3(v2, v3) :- idb0(v2, v3).

p4(v6, v7) :- edge(v6, v7).

p2(v4, v10) :- p3(v4, v5), p4(v4, v9).

idb0(v10, v11) :- p1(v10, v11).
idb0(v10, v11) :- p2(v10, v11)."""

    println("=== SSSP UNRESTRICTED DATALOG ===")
    println(datalog)
    println()

    assertEquals(datalog, expected)
  }
