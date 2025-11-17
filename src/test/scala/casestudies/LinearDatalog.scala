package test.casestudies

import linearfn.{ErrorMsg, HorizontalConstraint, RestrictedSelectable, VerticalConstraint, consumed, ops, repeatable, restricted, restrictedReturn, unrestricted}

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

// Query-level AST
@ops
class Query[A]():
  @repeatable
  def flatMap[B](@restrictedReturn f: Expr.Ref[A] => Query[B]): Query[B] =
    val ref = Expr.Ref[A]()
    Query.FlatMap(this, Expr.Fun(ref, f(ref)))

  @repeatable
  def map[B](@unrestricted f: Expr.Ref[A] => Expr[B]): Query[B] =
    val ref = Expr.Ref[A]()
    Query.Map(this, Expr.Fun(ref, f(ref)))

  @repeatable
  def withFilter(@unrestricted predicate: Expr.Ref[A] => Expr[Boolean]): Query[A] =
    val ref = Expr.Ref[A]()
    Query.Filter[A](this, Expr.Fun(ref, predicate(ref)))

  @repeatable
  def filter(@unrestricted predicate: Expr.Ref[A] => Expr[Boolean]): Query[A] =
    withFilter(predicate)

  @repeatable
  def union(that: Query[A]): Query[A] =
    Query.Union[A](this, that)

  @repeatable
  def unionAll(@restricted that: Query[A]): Query[A] =
    Query.Union[A](this, that)

  // Convert Query to IR and generate Datalog string
  def peek(): String =
    val resultPred = Query.freshPredName()
    val (program, _, _) = LinearDatalogImpl.translate(this, resultPred, scala.collection.immutable.Map.empty, 0)
    program.toDatalog

object Query:
  private var predCounter = 0
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
  case class IntensionalRef[A]() extends Query[A]:
    val id = intensionalRefCount  // Made public for LinearDatalogImpl
    intensionalRefCount += 1
    override def toString: String = s"IntensionalRef(id = ${id})"

  case class IntensionalPredicates[R](predicates: scala.collection.immutable.Map[IntensionalRef[Any], Query[Any]], idx: Int) extends Query[R]:
    override def toString: String = s"IntensionalPredicates(predicates = ${predicates}, idx = ${idx})"
  export RestrictedSelectable.RestrictedFn.strictApply as fix

  /**
   * customFix: Educational version showing how the library works.
   *
   * Base linearity constraints are imported from the library via the builder type.
   * The library's LinearFnBuilder type can only be constructed when base constraints
   * are satisfied, so requesting it via `using` enforces those constraints.
   *
   * We add ONLY 2 additional constraints here:
   * - Tuple.Size[QT] =:= Tuple.Size[RQT] (strictness)
   * - Tuple.Union[QT] <:< Query[?] (domain-specific)
   */
  def fixedPoint[QT <: Tuple, RQT <: Tuple](
    bases: QT
  )(fns: RestrictedSelectable.RestrictedFn.LinearFn[QT, RQT])(
    using
      builder: RestrictedSelectable.RestrictedFn.LinearFnBuilder[
        VerticalConstraint.Affine.type,
        HorizontalConstraint.ForAllRelevantForEachAffine.type,
        QT, RQT
      ],
      @implicitNotFound("customFix requires same number of args and returns")
      evStrict: Tuple.Size[QT] =:= Tuple.Size[RQT],
      @implicitNotFound("customFix requires all arguments to be Query types")
      evQuery: Tuple.Union[QT] <:< Query[?]
  ) = {
    val argsRefs = (0 until bases.size).map(_ => IntensionalRef[Any]())
    val restrictedRefs = argsRefs.map(a => RestrictedSelectable.makeRestrictedRef(() => a)).toArray
    val refsTuple = Tuple.fromArray(restrictedRefs).asInstanceOf[RestrictedSelectable.ToRestrictedRef[QT]]
    val exec = fns(refsTuple)
    val evaluatedT = RestrictedSelectable.tupleExecute(exec)
    val unioned = bases.toArray.zip(evaluatedT.toArray).map { (baseQ, evalQ) =>
      baseQ.asInstanceOf[Query[Any]].union(evalQ.asInstanceOf[Query[Any]])
    }
    val predicates = argsRefs.zip(unioned).toMap
    (0 to argsRefs.size).map(i => IntensionalPredicates(predicates, i))
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