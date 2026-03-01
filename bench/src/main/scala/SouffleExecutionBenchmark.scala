package bench

import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole
import java.util.concurrent.TimeUnit
import java.nio.file.{Files, Paths, StandardOpenOption}
import scala.sys.process.*

import test.casestudies.{Query, DatalogConnective, QueryOps, Expr}
import QueryOps.{given, *}
import Expr.{given, *}
import restrictedfn.RestrictedSelectable.{given, *}
import scala.NamedTuple.*

/**
 * Benchmarks for Souffle execution on generated Datalog.
 * Measures the time to run Souffle on the generated Datalog with sample data.
 */
@Fork(1)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
class SouffleExecutionBenchmark {

  val isMac = sys.props("os.name").toLowerCase.contains("mac")
  val souffleExecutable = if isMac then "souffle" else "/scratch/herlihy/souffle/build/src/souffle"

  /**
   * Convert Datalog output to valid Souffle format
   */
  def toSouffleFormat(datalog: String, edbRelations: Seq[String], outputRelation: String): String = {
    val fixed = datalog.replace(" == ", " = ")

    val predicateArityPattern = """(\w+)\(([^)]*)\)""".r
    val arityMap = scala.collection.mutable.Map[String, Int]()

    for (m <- predicateArityPattern.findAllMatchIn(fixed)) {
      val name = m.group(1)
      val args = m.group(2).split(",").map(_.trim).filter(_.nonEmpty)
      val arity = args.length
      arityMap(name) = arityMap.get(name).map(math.max(_, arity)).getOrElse(arity)
    }

    val idbPredicates = arityMap.keys.toSet -- edbRelations.toSet

    def makeDecl(name: String): String = {
      val arity = arityMap.getOrElse(name, 0)
      val args = (0 until arity).map(i => s"x$i: unsigned").mkString(", ")
      s".decl $name($args)"
    }

    val edbDecls = edbRelations.map(name => s"${makeDecl(name)}\n.input $name(IO=file, header=false)").mkString("\n")
    val idbDecls = idbPredicates.toSeq.sorted.map(makeDecl).mkString("\n")
    val outputDecl = s".output $outputRelation"

    s"""$edbDecls
       |$idbDecls
       |$outputDecl
       |
       |$fixed""".stripMargin
  }

  /**
   * Run Souffle on the given Datalog program and return output file path
   */
  def runSouffle(datalog: String, factsDir: String, edbRelations: Seq[String], outputRelation: String): String = {
    val souffleProgram = toSouffleFormat(datalog, edbRelations, outputRelation)

    val tempFile = Files.createTempFile("datalog_bench_", ".dl")
    Files.write(tempFile, souffleProgram.getBytes)

    val outputDir = Files.createTempDirectory("souffle_bench_output_")

    try {
      val command = s"$souffleExecutable -F $factsDir -D ${outputDir} ${tempFile}"
      val exitCode = command.!

      if exitCode != 0 then
        throw new RuntimeException(s"Souffle execution failed with exit code $exitCode")

      val outputFile = outputDir.resolve(s"$outputRelation.csv")
      outputFile.toString
    } finally {
      Files.deleteIfExists(tempFile)
    }
  }

  // ========== TRANSITIVE CLOSURE ==========

  @Benchmark
  def tc_restricted_souffleExecution(blackhole: Blackhole): Unit = {
    Query.intensionalRefCount = 0
    Query.predCounter = 0

    type IntRow = (i1: Int, i2: Int)
    val edges = Query.edb[IntRow]("edge", "i1", "i2")
    val path = Query.fixedPoint(Tuple1(edges))((pathTuple) =>
      DatalogConnective.apply(Tuple1(
        pathTuple._1.flatMap(p =>
          edges
            .filter(e => p.i2 == e.i1)
            .map(e => (i1 = p.i1, i2 = e.i2))
        )
      ))
    )._1

    val datalog = path.peek()

    val outputFile = runSouffle(datalog, "/tmp/tc-facts", Seq("edge"), "idb0")
    blackhole.consume(outputFile)
  }

  @Benchmark
  def tc_unrestricted_souffleExecution(blackhole: Blackhole): Unit = {
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

    val outputFile = runSouffle(datalog, "/tmp/tc-facts", Seq("edge"), "idb0")
    blackhole.consume(outputFile)
  }

  // ========== ANCESTRY ==========

  @Benchmark
  def ancestry_restricted_souffleExecution(blackhole: Blackhole): Unit = {
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
            .map(parent => (name = parent.child, gen = g.gen + Expr.ExprLit(1)))
        )
      ))
    )

    val generation = generationQuery._1
    val result = generation
      .filter(g => g.gen == Expr.ExprLit(2))
      .map(g => (name = g.name))

    val datalog = result.peek()

    val outputFile = runSouffle(datalog, "/tmp/ancestry-facts", Seq("parents"), "p0")
    blackhole.consume(outputFile)
  }

  @Benchmark
  def ancestry_unrestricted_souffleExecution(blackhole: Blackhole): Unit = {
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

    val outputFile = runSouffle(datalog, "/tmp/ancestry-facts", Seq("parents"), "p0")
    blackhole.consume(outputFile)
  }

  // ========== SSSP ==========

  @Benchmark
  def sssp_restricted_souffleExecution(blackhole: Blackhole): Unit = {
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
            .map(edge => (dst = edge.dst, cost = c.cost + edge.cost))
        )
      ))
    )

    val result = costQuery._1
    val datalog = result.peek()

    val outputFile = runSouffle(datalog, "/Users/anna/lamp/carac/src/test/scala/test/examples/rqb_sssp/souffle-facts", Seq("base", "edge"), "idb0")
    blackhole.consume(outputFile)
  }

  @Benchmark
  def sssp_unrestricted_souffleExecution(blackhole: Blackhole): Unit = {
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

    val outputFile = runSouffle(datalog, "/Users/anna/lamp/carac/src/test/scala/test/examples/rqb_sssp/souffle-facts", Seq("base", "edge"), "idb0")
    blackhole.consume(outputFile)
  }
}
