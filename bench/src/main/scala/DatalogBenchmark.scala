package bench

import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole
import java.util.concurrent.TimeUnit

import test.casestudies.{Query, DatalogConnective, QueryOps, Expr}
import QueryOps.{given, *}
import restrictedfn.RestrictedSelectable.{given, *}
import scala.NamedTuple.*

type IntRow = (i1: Int, i2: Int)

@Fork(1)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
class DatalogBenchmark {

  @Benchmark
  def linearRecursiveQuery_restricted(blackhole: Blackhole): Unit = {
    // Linear recursive query test case from LinearDatalogTest using restricted functions
    val q1 = Query.edb[IntRow]("q1")
    val q2 = Query.edb[IntRow]("q2")
    val q3 = Query.edb[IntRow]("q3")
    val r = Query.fixedPoint(q1, q2.union(q3))((a1, a2) =>
      val r1 = a1.union(q3)
      val r2 = a2.map(e => (i1 = e.i1, i2 = e.i2))
      DatalogConnective.apply((r1, r2))
    )
    blackhole.consume(r)
  }

  @Benchmark
  def linearRecursiveQuery_unrestricted(blackhole: Blackhole): Unit = {
    // Same query but using unrestrictedFixedPoint (no restricted function overhead)
    val q1 = Query.edb[IntRow]("q1")
    val q2 = Query.edb[IntRow]("q2")
    val q3 = Query.edb[IntRow]("q3")
    val r = Query.unrestrictedFixedPoint(q1, q2.union(q3))((a1, a2) =>
      val r1 = a1.union(q3)
      val r2 = a2.map(e => (i1 = e.i1, i2 = e.i2))
      (r1, r2)
    )
    blackhole.consume(r)
  }

  // ========== ANCESTRY BENCHMARK ==========

  @Benchmark
  def ancestry_restricted(blackhole: Blackhole): Unit = {
    type Parent = (parent: String, child: String)

    val parents = Query.edb[Parent]("parents")
    val base = parents
      .filter(p => p.child == Expr.ExprLit(1))
      .map(p => Expr.Project((name = p.child, gen = Expr.ExprLit(1))))

    val generationQuery = Query.fixedPoint(Tuple1(base))(genTuple =>
      DatalogConnective.apply(Tuple1(
        genTuple._1.flatMap(g =>
          parents
            .filter(parent => parent.parent == g.name)
            .map(parent => Expr.Project((name = parent.child, gen = g.gen + Expr.ExprLit(1))))
        )
      ))
    )

    val generation = generationQuery._1
    val result = generation
      .filter(g => g.gen == Expr.ExprLit(2))
      .map(g => Expr.Project((name = g.name)))

    blackhole.consume(result)
  }

  @Benchmark
  def ancestry_unrestricted(blackhole: Blackhole): Unit = {
    type Parent = (parent: String, child: String)

    val parents = Query.edb[Parent]("parents")
    val base = parents
      .filter(p => p.child == Expr.ExprLit(1))
      .map(p => Expr.Project((name = p.child, gen = Expr.ExprLit(1))))

    val generationQuery = Query.unrestrictedFixedPoint(Tuple1(base))(genTuple =>
      Tuple1(
        genTuple._1.flatMap(g =>
          parents
            .filter(parent => parent.parent == g.name)
            .map(parent => Expr.Project((name = parent.child, gen = g.gen + Expr.ExprLit(1))))
        )
      )
    )

    val generation = generationQuery._1
    val result = generation
      .filter(g => g.gen == Expr.ExprLit(2))
      .map(g => Expr.Project((name = g.name)))

    blackhole.consume(result)
  }

  // ========== SSSP BENCHMARK ==========

  @Benchmark
  def sssp_restricted(blackhole: Blackhole): Unit = {
    type Edge = (src: Int, dst: Int, cost: Int)
    type Cost = (dst: Int, cost: Int)

    val baseEDB = Query.edb[Cost]("base")
    val edgeEDB = Query.edb[Edge]("edge")

    val costQuery = Query.fixedPoint(Tuple1(baseEDB))(costTuple =>
      DatalogConnective.apply(Tuple1(
        costTuple._1.flatMap(c =>
          edgeEDB
            .filter(edge => edge.src == c.dst)
            .map(edge => Expr.Project((dst = edge.dst, cost = c.cost + edge.cost)))
        )
      ))
    )

    blackhole.consume(costQuery._1)
  }

  @Benchmark
  def sssp_unrestricted(blackhole: Blackhole): Unit = {
    type Edge = (src: Int, dst: Int, cost: Int)
    type Cost = (dst: Int, cost: Int)

    val baseEDB = Query.edb[Cost]("base")
    val edgeEDB = Query.edb[Edge]("edge")

    val costQuery = Query.unrestrictedFixedPoint(Tuple1(baseEDB))(costTuple =>
      Tuple1(
        costTuple._1.flatMap(c =>
          edgeEDB
            .filter(edge => edge.src == c.dst)
            .map(edge => Expr.Project((dst = edge.dst, cost = c.cost + edge.cost)))
        )
      )
    )

    blackhole.consume(costQuery._1)
  }
}
