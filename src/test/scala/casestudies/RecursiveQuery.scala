package test.casestudies

import linearfn.{RestrictedSelectable, consumed, ops, unrestricted, restrictedFn, repeatable}

import scala.NamedTuple.AnyNamedTuple

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
  def flatMap[B](@restrictedFn f: Expr.Ref[A] => Query[B]): Query[B] =
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

object Query:
  case class Filter[A]($from: Query[A], $pred: Expr.Fun[A, Expr[Boolean]]) extends Query[A]
  case class Map[A, B]($from: Query[A], $query: Expr.Fun[A, Expr[B]]) extends Query[B]
  case class FlatMap[A, B]($from: Query[A], $query: Expr.Fun[A, Query[B]]) extends Query[B]
  case class Union[A]($left: Query[A], $right: Query[A]) extends Query[A]
  case class RecursiveQuery[Q <: Tuple](queries: Q) extends Query[Any]
  export RestrictedSelectable.LinearFn.strictApply as fix

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
