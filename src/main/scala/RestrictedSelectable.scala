package linearfn

import Utils.*
import scala.language.implicitConversions
import scala.language.reflectiveCalls
import NamedTuple.{AnyNamedTuple, NamedTuple}
import scala.annotation.implicitNotFound
import scala.reflect.Selectable.reflectiveSelectable

/**
 * Use Selectable to proxy field/method access.
 * TODO: for applyDynamic to work, seems the user needs to manually declare methods somewhere, not sure if there's a way to do that without leaking Restricted.
 * Good because type safe, but doesn't work out of the box with non-product types, and requires users to define functions.
 */
object RestrictedSelectable extends RestrictedFnBase:

  // Implementation-specific Restricted trait - extends RestrictedBase
  trait Restricted[A, D <: Tuple, C <: Tuple] extends RestrictedBase[A, D, C], Selectable:
    type Fields = NamedTuple.Map[NamedTuple.From[A], [T] =>> Restricted[T, D, C]]
    def stageField(name: String): Restricted[A, D, C]
    def stageCall[R, D2 <: Tuple, C2 <: Tuple](name: String, args: Tuple): Restricted[R, D2, C2]

    def selectDynamic(name: String) = {
      stageField(name)
    }

    // TODO: for now unused
    def applyDynamic(method: String)(): Restricted[A, D, C] = {
      stageCall[A, D, C](method, EmptyTuple)
    }

    // TODO: for now unused
    def applyDynamic[T1](method: String)(arg: T1): Restricted[A, CollateDeps[T1, D], CollateConsumed[T1, C]] = { // TODO:
      stageCall[A, CollateDeps[T1, D], CollateConsumed[T1, C]](method, Tuple1(arg))
    }

    def execute(): A

  object Restricted:
    case class RestrictedRef[A, D <: Tuple, C <: Tuple](protected val fn: () => A) extends Restricted[A, D, C]:
      def execute(): A = fn()

      override def stageField(name: String) =
        RestrictedRef(() =>
          val obj = fn()
          val field = obj.getClass.getDeclaredField(name)
          field.setAccessible(true)
          field.get(obj).asInstanceOf[A]
        )

      override def stageCall[R, D2 <: Tuple, C2 <: Tuple](name: String, args: Tuple): Restricted[R, D2, C2] = {
        RestrictedRef[R, D2, C2](() =>
          val obj = fn()

          // Helper to unwrap Restricted values from function return types
          // This is needed because @restrictedReturn can appear on function parameters like:
          //   def flatMap[B](@restrictedReturn f: A => Query[B]): Query[B]
          // The generated extension receives: f: A => Restricted[Query[B], D, C]
          // But the actual method expects: f: A => Query[B]
          // So we wrap the function to unwrap its Restricted return value
          def unwrapNested(arg: Any): Any = arg match {
            case f: Function1[_, _] =>
              (a: Any) => {
                val result = f.asInstanceOf[Any => Any](a)
                result match {
                  case r: Restricted[_, _, _] => r.execute()
                  case other => other
                }
              }
            case other => other
          }

          // Execute any Restricted arguments to get their actual values
          val executedArgs = args.productIterator.map {
            case r: Restricted[_, _, _] => r.execute()
            case other => unwrapNested(other)
          }.toSeq

          // Get all methods with this name (including inherited methods)
          val candidates = obj.getClass.getMethods.filter(_.getName == name)

          val method = candidates match {
            case Array(single) =>
              // Only one method with this name - just use it
              single
            case multiple if multiple.nonEmpty =>
              // Multiple overloads - need to match by parameter count
              multiple.find(_.getParameterCount == executedArgs.length).getOrElse {
                throw new NoSuchMethodException(
                  s"No method found: ${obj.getClass.getName}.$name with ${executedArgs.length} parameters"
                )
              }
            case _ =>
              throw new NoSuchMethodException(s"No method found: ${obj.getClass.getName}.$name")
          }

          // Method.invoke handles primitive/wrapper boxing automatically
          method.invoke(obj, executedArgs*).asInstanceOf[R]
        )
      }

    // Implicit conversion to allow plain values where Restricted is expected
    given [S]: Conversion[S, Restricted[S, EmptyTuple, EmptyTuple]] with
      def apply(value: S): Restricted[S, EmptyTuple, EmptyTuple] =
        RestrictedRef[S, EmptyTuple, EmptyTuple](() => value)

    /**
     * Type class for user-defined liftable containers.
     * Implement this trait to enable lifting for custom container types.
     *
     * Built-in containers (List, Option, Vector) are lifted automatically via match types.
     * User-defined containers require explicit `.lift` calls.
     */
    trait Liftable[F[_]]:
      def map[A, B](fa: F[A])(f: A => B): F[B]

    /**
     * Extension method to explicitly lift user-defined containers.
     * Transforms F[Restricted[A, D, C]] into Restricted[F[A], D, C].
     *
     * Usage:
     * {{{
     * case class Box[T](contents: T)
     * given Liftable[Box] with
     *   def map[A, B](fa: Box[A])(f: A => B) = Box(f(fa.contents))
     *
     * val result = RestrictedFn.apply((ex1, ex2))(refs =>
     *   (Box(refs._1).lift, refs._2)  // .lift normalizes the type
     * )
     * }}}
     */
    extension [F[_], A, D <: Tuple, C <: Tuple](container: F[Restricted[A, D, C]])(using ev: Liftable[F])
      def lift: Restricted[F[A], D, C] =
        RestrictedRef[F[A], D, C](() => ev.map(container)(_.execute()))

  // Implement abstract methods from RestrictedFnBase
  protected def makeRestrictedRef[A, D <: Tuple, C <: Tuple](fn: () => A): Restricted[A, D, C] =
    Restricted.RestrictedRef(fn)