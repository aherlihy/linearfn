package test.casestudies

import linearfn.{RestrictedSelectable, consumed, ops, unrestricted, restricted, restrictedReturn, repeatable, VerticalConstraint, HorizontalConstraint, ErrorMsg}

import scala.NamedTuple.AnyNamedTuple
import scala.annotation.implicitNotFound

/**
 * Case Study: LINQ-style Recursive queries
 * Problem: Require linear recursion
 */


// Column-level AST
trait Expr[Result] extends Selectable:
  type Fields = NamedTuple.Map[NamedTuple.From[Result], Expr]
  def selectDynamic(fieldName: String) = Expr.Select(this, fieldName)
object Expr:
  type StripExpr[E] = E match
    case Expr[b] => b

  case class Select[A]($x: Expr[A], $name: String) extends Expr[A]
  case class Project[A <: AnyNamedTuple]($a: A) extends Expr[NamedTuple.Map[A, StripExpr]]

  private var refCount = 0
  case class Ref[A](idx: Int = -1) extends Expr[A]:
    private val $id = refCount
    refCount += 1

  case class Fun[A, B]($param: Ref[A], $body: B)

  case class Plus($x: Expr[Int], $y: Expr[Int]) extends Expr[Int]
  extension(x: Expr[Int])
    def +(y: Expr[Int]): Expr[Int] = Plus(x, y)

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
  case class Filter[A]($from: Query[A], $pred: Expr.Fun[A, Expr[Boolean]]) extends Query[A]
  case class Map[A, B]($from: Query[A], $query: Expr.Fun[A, Expr[B]]) extends Query[B]
  case class FlatMap[A, B]($from: Query[A], $query: Expr.Fun[A, Query[B]]) extends Query[B]
  case class Union[A]($left: Query[A], $right: Query[A]) extends Query[A]
  case class RecursiveQuery[Q <: Tuple](queries: Q) extends Query[Any]
  export RestrictedSelectable.RestrictedFn.strictApply as fix

  /**
   * simpleFix: Most ergonomic API!
   *
   * Only TWO type parameters: QT (arguments) and RQT (returns).
   * Uses the library's LinearFn type alias.
   *
   * Example: Query.simpleFix(q1, q2)((a1, a2) => (a1, a2))
   */
  def simpleFix[QT <: Tuple, RQT <: Tuple](
    bases: QT
  )(fns: RestrictedSelectable.RestrictedFn.LinearFn[QT, RQT])(
    using
      // Base linearity constraints - enforced by requesting the builder
      builder: RestrictedSelectable.RestrictedFn.LinearFnBuilder[
        VerticalConstraint.Affine.type,
        HorizontalConstraint.ForAllRelevantForEachAffine.type,
        QT, RQT
      ],
      // Additional domain-specific constraints
      @implicitNotFound("simpleFix requires same number of args and returns")
      evStrict: Tuple.Size[QT] =:= Tuple.Size[RQT],
      @implicitNotFound("simpleFix requires all arguments to be Query types")
      evQuery: Tuple.Union[QT] <:< Query[?]
  ): RQT =
    // User code decides how to handle the result - in this case, cast to QT
    builder.execute(bases)(fns)

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
  def customFix[QT <: Tuple, RQT <: Tuple](
    bases: QT
  )(fns: RestrictedSelectable.RestrictedFn.LinearFn[QT, RQT])(
    using
      // Request the library's builder - this enforces ALL base linearity constraints
      builder: RestrictedSelectable.RestrictedFn.LinearFnBuilder[
        VerticalConstraint.Affine.type,
        HorizontalConstraint.ForAllRelevantForEachAffine.type,
        QT, RQT
      ],
      // ONLY additional constraints - NO base linearity constraints
      @implicitNotFound("customFix requires same number of args and returns")
      evStrict: Tuple.Size[QT] =:= Tuple.Size[RQT],
      @implicitNotFound("customFix requires all arguments to be Query types")
      evQuery: Tuple.Union[QT] <:< Query[?]
  ): RQT =
    builder.execute(bases)(fns)


//object QueryOps:
//  extension [A, D <: Tuple, C <: Tuple](p: RestrictedSelectable.Restricted[Query[A], D, EmptyTuple])
//    def flatMap[B, D1 <: Tuple, C1 <: Tuple](f: Expr.Ref[A] => RestrictedSelectable.Restricted[Query[B], D1, C1]): RestrictedSelectable.Restricted[Query[B], Tuple.Concat[D1, D], EmptyTuple] =
//      p.stageCall[Query[B], Tuple.Concat[D1, D], EmptyTuple]("flatMap", Tuple1(f))
//
//    def map[B, D1 <: Tuple, C1 <: Tuple](f: Expr.Ref[A] => Expr[B]): RestrictedSelectable.Restricted[Query[B], D1, EmptyTuple] =
//      p.stageCall[Query[B], D1, EmptyTuple]("map", Tuple1(f))
//
//    def withFilter[D1 <: Tuple, C1 <: Tuple](p2: Expr.Ref[A] => Expr[Boolean]): RestrictedSelectable.Restricted[Query[A], D1, EmptyTuple] =
//      p.stageCall[Query[A], D1, EmptyTuple]("withFilter", Tuple1(p))
//
//    def union[D1 <: Tuple, C1 <: Tuple](p2: RestrictedSelectable.Restricted[Query[A], D1, C1]): RestrictedSelectable.Restricted[Query[A], Tuple.Concat[D1, D], EmptyTuple] =
//      p.stageCall[Query[A], Tuple.Concat[D1, D], EmptyTuple]("union", Tuple1(p2))
