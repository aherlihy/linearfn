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
  extension(x: Expr[Int])
    def +(y: Expr[Int]): Expr[Int] = Plus(x, y)

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
  def union(that: Query[A]): Query[A] =
    Query.Union[A](this, that)

  @repeatable
  def unionAll(@restricted that: Query[A]): Query[A] =
    Query.Union[A](this, that)

object Query:
  case class EDB[A](predicate: String) extends Query[A]:
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
    private val idx = intensionalRefCount
    intensionalRefCount += 1
    override def toString: String = s"IntensionalRef(id = ${idx})"

  case class IntensionalPredicates[R](//TODO!)
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
  ) = ???
