package bench

import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole
import java.util.concurrent.TimeUnit
import java.nio.file.{Files, Paths}

import test.casestudies.{Query, DatalogConnective, QueryOps, Expr}
import QueryOps.{given, *}
import Expr.{given, *}
import restrictedfn.RestrictedSelectable.{given, *}
import scala.NamedTuple.*

/**
 * Benchmarks for Datalog generation from DSL queries.
 * Measures the time to generate Datalog IR and convert to string.
 */
@Fork(1)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
class DatalogGenerationBenchmark {

  // ========== TRANSITIVE CLOSURE ==========

  @Benchmark
  def tc_restricted(blackhole: Blackhole): Unit = {
    Query.intensionalRefCount = 0
    Query.predCounter = 0

    type IntRow = (i1: Int, i2: Int)
    val edges = Query.edb[IntRow]("edge", "i1", "i2")
    val path = Query.fixedPoint(Tuple1(edges))((pathTuple) =>
      DatalogConnective.apply(Tuple1(
        pathTuple._1.flatMap(p =>
          edges
            .filter(e => p.i2 == e.i1)
            .map(e => (i1 = p.i1, i2 = e.i2).toRow)
        )
      ))
    )._1

    val datalog = path.peek()
    blackhole.consume(datalog)
  }

  @Benchmark
  def tc_unrestricted(blackhole: Blackhole): Unit = {
    Query.intensionalRefCount = 0
    Query.predCounter = 0

    type IntRow = (i1: Int, i2: Int)
    val edges = Query.edb[IntRow]("edge", "i1", "i2")
    val path = Query.unrestrictedFixedPoint(Tuple1(edges))((pathTuple) =>
      Tuple1(
        pathTuple._1.flatMap(p =>
          edges
            .filter(e => p.i2 == e.i1)
            .map(e => (i1 = p.i1, i2 = e.i2).toRow)
        )
      )
    )._1

    val datalog = path.peek()
    blackhole.consume(datalog)
  }

  // ========== ANCESTRY ==========

  @Benchmark
  def ancestry_restricted(blackhole: Blackhole): Unit = {
    Query.intensionalRefCount = 0
    Query.predCounter = 0

    type Parent = (parent: Int, child: Int)
    type Generation = (name: Int, gen: Int)

    val parents = Query.edb[Parent]("parents", "parent", "child")

    val base = parents
      .filter(p => p.parent == Expr.ExprLit(1))
      .map(p => (name = p.child, gen = Expr.ExprLit(1)))

    val generationQuery = Query.fixedPoint(Tuple1(base))(genTuple =>
      DatalogConnective.apply(Tuple1(
        genTuple._1.flatMap(g =>
          parents
            .filter(parent => parent.parent == g.name)
            .map(parent => (name = parent.child, gen = g.gen + Expr.ExprLit(1)).toRow)
        )
      ))
    )

    val generation = generationQuery._1
    val result = generation
      .filter(g => g.gen == Expr.ExprLit(2))
      .map(g => (name = g.name))

    val datalog = result.peek()
    blackhole.consume(datalog)
  }

  @Benchmark
  def ancestry_unrestricted(blackhole: Blackhole): Unit = {
    Query.intensionalRefCount = 0
    Query.predCounter = 0

    type Parent = (parent: Int, child: Int)
    type Generation = (name: Int, gen: Int)

    val parents = Query.edb[Parent]("parents", "parent", "child")

    val base = parents
      .filter(p => p.parent == Expr.ExprLit(1))
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
    blackhole.consume(datalog)
  }

  // ========== SSSP ==========

  @Benchmark
  def sssp_restricted(blackhole: Blackhole): Unit = {
    Query.intensionalRefCount = 0
    Query.predCounter = 0

    type Edge = (src: Int, dst: Int, cost: Int)
    type Cost = (dst: Int, cost: Int)

    val baseEDB = Query.edb[Cost]("base", "dst", "cost")
    val edgeEDB = Query.edb[Edge]("edge", "src", "dst", "cost")

    val costQuery = Query.fixedPoint(Tuple1(baseEDB))(costTuple =>
      DatalogConnective.apply(Tuple1(
        costTuple._1.flatMap(c =>
          edgeEDB
            .filter(edge => edge.src == c.dst)
            .map(edge => (dst = edge.dst, cost = c.cost + edge.cost).toRow)
        )
      ))
    )

    val result = costQuery._1
    val datalog = result.peek()
    blackhole.consume(datalog)
  }

  @Benchmark
  def sssp_unrestricted(blackhole: Blackhole): Unit = {
    Query.intensionalRefCount = 0
    Query.predCounter = 0

    type Edge = (src: Int, dst: Int, cost: Int)
    type Cost = (dst: Int, cost: Int)

    val baseEDB = Query.edb[Cost]("base", "dst", "cost")
    val edgeEDB = Query.edb[Edge]("edge", "src", "dst", "cost")

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
    blackhole.consume(datalog)
  }
}
