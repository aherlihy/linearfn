package test.casestudies

import restrictedfn.{ErrorMsg, Multiplicity, RestrictedSelectable, ops, restricted, restrictedReturn, unrestricted}

import scala.annotation.implicitNotFound

/**
 * Case Study: Linear Datalog
 */

// Column-level AST
trait Expr[Result] extends Selectable:
  type Fields = NamedTuple.Map[NamedTuple.From[Result], Expr]
  def selectDynamic(fieldName: String) = Expr.Select(this, fieldName)
  def ==(y: Expr[?]): Expr[Boolean] = Expr.Eq(this, y)
  
object Expr:
  type StripExpr[E] = E match
    case Expr[b] => b

  case class Select[A]($x: Expr[A], $name: String) extends Expr[A]
  case class Project[A <: NamedTuple.AnyNamedTuple]($a: A) extends Expr[NamedTuple.Map[A, StripExpr]]
  type IsTupleOfExpr[A <: NamedTuple.AnyNamedTuple] = Tuple.Union[NamedTuple.DropNames[A]] <:< Expr[?]

  given [A <: NamedTuple.AnyNamedTuple : IsTupleOfExpr]: Conversion[A, Project[A]] with
    def apply(x: A): Project[A] = Project(x)

  private var refCount = 0
  case class Ref[A]() extends Expr[A]:
    private val $id = refCount
    refCount += 1
    override def toString: String = s"Ref(id = ${$id})"

  case class Fun[A, B]($param: Ref[A], $body: B)

  case class Plus($x: Expr[Int], $y: Expr[Int]) extends Expr[Int]
  case class Eq($x: Expr[?], $y: Expr[?]) extends Expr[Boolean]
  case class And($x: Expr[Boolean], $y: Expr[Boolean]) extends Expr[Boolean]
  extension(x: Expr[Int])
    def +(y: Expr[Int]): Expr[Int] = Plus(x, y)
  extension(x: Expr[Boolean])  
    def &&(y: Expr[Boolean]): Expr[Boolean] = And(x, y)

  case class ExprLit(i: Int) extends Expr[Int]

/**
 * DatalogConnective: Type alias for the connective used in linear datalog.
 * RQT (wrapped tuple) is the tuple of restricted values.
 * DT (dependency tuple) is inferred from the actual dependencies in the composed values.
 */
type DatalogConnective[RQT <: Tuple] = RestrictedSelectable.CustomConnective[RQT, Multiplicity.Affine, Multiplicity.Relevant]
object DatalogConnective:
  def apply[RQT <: Tuple](values: RQT) =
//  (using ev: DT =:= RestrictedSelectable.ExtractDependencyTypes[RT]): DatalogConnective[RT, DT] =
    RestrictedSelectable.CustomConnective[RQT, Multiplicity.Affine, Multiplicity.Relevant](values)
// Query-level AST
@ops
class Query[A]():
  def flatMap[B](@restrictedReturn f: Expr.Ref[A] => Query[B]): Query[B] =
    val ref = Expr.Ref[A]()
    Query.FlatMap(this, Expr.Fun(ref, f(ref)))

  def map[B](@unrestricted f: Expr.Ref[A] => Expr[B]): Query[B] =
    val ref = Expr.Ref[A]()
    Query.Map(this, Expr.Fun(ref, f(ref)))

  def withFilter(@unrestricted predicate: Expr.Ref[A] => Expr[Boolean]): Query[A] =
    val ref = Expr.Ref[A]()
    Query.Filter[A](this, Expr.Fun(ref, predicate(ref)))

  def filter(@unrestricted predicate: Expr.Ref[A] => Expr[Boolean]): Query[A] =
    withFilter(predicate)

  def union(that: Query[A]): Query[A] =
    Query.Union[A](this, that)

  def unionAll(@restricted that: Query[A]): Query[A] =
    Query.Union[A](this, that)

  // Convert Query to IR and generate Datalog string
  def peek(): String =
    val resultPred = Query.freshPredName()
    val (program, _, _) = LinearDatalogImpl.translate(this, resultPred, scala.collection.immutable.Map.empty, 0)
    program.toDatalog

object Query:
  var predCounter = 0  // Made public for testing
  def freshPredName(): String =  // Made public for LinearDatalogImpl
    val name = s"p$predCounter"
    predCounter += 1
    name

  case class EDB[A](predicate: String, arity: Int = 2) extends Query[A]:
    override def toString: String = s"EDB(predicate = ${predicate})"

  // Factory method that returns Query[A] instead of EDB[A]
  def edb[A](predicate: String): Query[A] = EDB[A](predicate)

  case class Filter[A]($from: Query[A], $pred: Expr.Fun[A, Expr[Boolean]]) extends Query[A]:
    override def toString: String = s"Filter(from = ${$from}, pred = ${$pred})"

  case class Map[A, B]($from: Query[A], $query: Expr.Fun[A, Expr[B]]) extends Query[B]:
    override def toString: String = s"Map(from = ${$from}, query = ${$query})"

  case class FlatMap[A, B]($from: Query[A], $query: Expr.Fun[A, Query[B]]) extends Query[B]:
    override def toString: String = s"FlatMap(from = ${$from}, query = ${$query})"


  case class Union[A]($left: Query[A], $right: Query[A]) extends Query[A]:
    override def toString: String = s"Union(left = ${$left}, right = ${$right})"

  var intensionalRefCount = 0
  private def freshIntensionalId(): Int =
    val id = intensionalRefCount
    intensionalRefCount += 1
    id

  case class IntensionalRef[A](id: Int) extends Query[A]:
    override def toString: String = s"IntensionalRef(id = ${id})"

  case class IntensionalPredicates[R](predicates: scala.collection.immutable.Map[IntensionalRef[Any], Query[Any]], idx: Int) extends Query[R]:
    override def toString: String = s"IntensionalPredicates(predicates = ${predicates}, idx = ${idx})"


  // Helper type to extract row types from Query tuple
  // This is domain-specific to linear datalog: ensures return Queries have same row types as argument Queries
  type ExtractQueryRowTypes[T <: Tuple] <: Tuple = T match
    case EmptyTuple => EmptyTuple
    case Query[r] *: tail => r *: ExtractQueryRowTypes[tail]


  /**
   * fixedPoint: Fixed-point operator for linear datalog.
   *
   * Enforces both linearity constraints (via builder) and fixedPoint-specific constraints:
   * - Same number of arguments and returns (strictness)
   * - Return types match argument types (type safety for recursive fixed-point)
   */
  def fixedPoint[QT <: Tuple, RQT <: Tuple, DT <: Tuple](
    bases: QT
  )(fns: RestrictedSelectable.RestrictedFn.RestrictedFn[QT, DatalogConnective[RQT]])(
    using
      builder: RestrictedSelectable.RestrictedFn.RestrictedFnBuilder[
        QT,
        DatalogConnective[RQT]
      ],
      @implicitNotFound("fixedPoint requires same number of arguments and returns")
      evStrict: Tuple.Size[RQT] =:= Tuple.Size[QT],
      @implicitNotFound("fixedPoint requires return types to match argument types")
      evReturnTypes: ExtractQueryRowTypes[RestrictedSelectable.ExtractResultTypes[RQT]] =:= ExtractQueryRowTypes[QT]
  ): QT = {
    val argsRefs = (0 until bases.size).map(_ => IntensionalRef[Any](freshIntensionalId()))
    val restrictedRefs = argsRefs.map(a => RestrictedSelectable.makeRestrictedRef(() => a)).toArray
    val restrictedRefsTuple = Tuple.fromArray(restrictedRefs).asInstanceOf[RestrictedSelectable.ToRestrictedRef[QT]]
    val resultConnective = fns(restrictedRefsTuple)
    val evaluated = resultConnective.execute()  // Execute the composed connective to get the tuple of results
    val unioned = bases.toArray.zip(evaluated.toArray).map { (baseQ, evalQ) =>
      baseQ.asInstanceOf[Query[Any]].union(evalQ.asInstanceOf[Query[Any]])
    }
    val predicates = argsRefs.zip(unioned).toMap
    Tuple.fromArray((0 until argsRefs.size).map(i => IntensionalPredicates(predicates, i)).toArray).asInstanceOf[QT]
  }

// Intermediate Representation
object IR:
  case class Program(predicates: Map[String, PredicateDef]):
    def toDatalog: String =
      predicates.values.map(_.toDatalog).mkString("\n\n")

  case class PredicateDef(name: String, rules: List[Rule]):
    def toDatalog: String =
      rules.map(_.toDatalog(name)).mkString("\n")

  case class Rule(head: Atom, body: List[Atom], constraints: List[Constraint] = List.empty):
    def toDatalog(predicateName: String): String =
      val headStr = s"$predicateName(${head.terms.map(_.toDatalog).mkString(", ")})"
      if body.isEmpty && constraints.isEmpty then
        s"$headStr."
      else
        val bodyParts = body.map(atom => s"${atom.predicate}(${atom.terms.map(_.toDatalog).mkString(", ")})")
        val constraintParts = constraints.map(_.toDatalog)
        val allParts = bodyParts ++ constraintParts
        val bodyStr = allParts.mkString(", ")
        s"$headStr :- $bodyStr."

  case class Atom(predicate: String, terms: List[Term])

  sealed trait Constraint:
    def toDatalog: String

  case class Eq(left: Term, right: Term) extends Constraint:
    def toDatalog: String = s"${left.toDatalog} == ${right.toDatalog}"

  sealed trait Term:
    def toDatalog: String

  case class Var(name: String) extends Term:
    def toDatalog: String = name

  case class Const(value: String) extends Term:
    def toDatalog: String = value