package test.casestudies

import restrictedfn.RestrictedSelectable

/**
 * Explicit extension methods for Query that bypass stageCall.
 * These methods directly implement the operations inline instead of
 * going through stageCall, to measure the overhead of stageCall.
 *
 * Key insight: When fixedPoint wraps Query values in Restricted, and those
 * Query values themselves came from restricted operations, we get nested
 * Restricted[Restricted[Query[A], D1], D2]. The extension methods need to
 * handle this by recursively unwrapping to the underlying Query.
 */
object QueryExplicitOps:

  extension [A, D <: Tuple](self: RestrictedSelectable.Restricted[Query[A], D])
    def flatMap_explicit[B, D1 <: Tuple](f: Expr.Ref[A] => RestrictedSelectable.Restricted[Query[B], D1]): RestrictedSelectable.Restricted[Query[B], Tuple.Concat[D1, D]] =
      val ref = Expr.Ref[A]()
      RestrictedSelectable.Restricted.RestrictedRef(() =>
        Query.FlatMap(self.execute(), Expr.Fun(ref, f(ref).execute()))
      )

  extension [A, D <: Tuple](self: RestrictedSelectable.Restricted[Query[A], D])
    def map_explicit[B](f: Expr.Ref[A] => Expr[B]): RestrictedSelectable.Restricted[Query[B], D] =
      val ref = Expr.Ref[A]()
      RestrictedSelectable.Restricted.RestrictedRef(() =>
        Query.Map(self.execute(), Expr.Fun(ref, f(ref)))
      )

  extension [A, D <: Tuple](self: RestrictedSelectable.Restricted[Query[A], D])
    def filter_explicit(predicate: Expr.Ref[A] => Expr[Boolean]): RestrictedSelectable.Restricted[Query[A], D] =
      val ref = Expr.Ref[A]()
      RestrictedSelectable.Restricted.RestrictedRef(() =>
        Query.Filter[A](self.execute(), Expr.Fun(ref, predicate(ref)))
      )

  extension [A, D <: Tuple](self: RestrictedSelectable.Restricted[Query[A], D])
    def union_explicit[D1 <: Tuple](that: RestrictedSelectable.Restricted[Query[A], D1]): RestrictedSelectable.Restricted[Query[A], Tuple.Concat[D1, D]] =
      RestrictedSelectable.Restricted.RestrictedRef(() =>
        Query.Union[A](self.execute(), that.execute())
      )
