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
object RestrictedSelectable extends LinearFnBase:

  // Implementation-specific Restricted trait
  trait Restricted[A, D <: Tuple] extends Selectable:
    type Fields = NamedTuple.Map[NamedTuple.From[A], [T] =>> Restricted[T, D]]
    def stageField(name: String): Restricted[A, D]
    def stageCall[R, D2 <: Tuple](name: String, args: Tuple): Restricted[R, D2]

    def selectDynamic(name: String) = {
      println(s"field access $name")
      stageField(name)
    }

    def applyDynamic(method: String)(): Restricted[A, D] = {
      println(s"applying $method with no args")
      stageCall[A, D](method, EmptyTuple)
    }

    def applyDynamic[T1](method: String)(arg: T1): Restricted[A, CollateDeps[T1, D]] = {
      println(s"applying $method with arg: $arg")
      stageCall[A, CollateDeps[T1, D]](method, Tuple1(arg))
    }

    def execute(): A

  object Restricted:
    case class LinearRef[A, D <: Tuple](protected val fn: () => A) extends Restricted[A, D]:
      def execute(): A = fn()

      override def stageField(name: String) =
        LinearRef(() =>
          println(s"inside fn: staged field access $name")
          val obj = fn()
          val field = obj.getClass.getDeclaredField(name)
          field.setAccessible(true)
          field.get(obj).asInstanceOf[A]
        )

      override def stageCall[R, D2 <: Tuple](name: String, args: Tuple): Restricted[R, D2] = {
        println(s"staging call $name with args: $args")
        LinearRef[R, D2](() =>
          println(s"inside fn: staged call $name with args: $args")
          val obj = fn()

          // Execute any Restricted arguments to get their actual values
          val executedArgs = args.productIterator.map {
            case r: Restricted[_, _] => r.execute()
            case other => other
          }.toSeq

          val method = if (executedArgs.isEmpty) {
            obj.getClass.getDeclaredMethod(name)
          } else {
            val argClasses = executedArgs.map(_.getClass.asInstanceOf[Class[?]]).toArray
            obj.getClass.getDeclaredMethod(name, argClasses*)
          }
          method.invoke(obj, executedArgs*).asInstanceOf[R]
        )
      }

    // Implicit conversion to allow plain values where Restricted is expected
    given [S]: Conversion[S, Restricted[S, EmptyTuple]] with
      def apply(value: S): Restricted[S, EmptyTuple] =
        LinearRef[S, EmptyTuple](() => value)

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
     * Transforms F[Restricted[A, D]] into Restricted[F[A], D].
     *
     * Usage:
     * {{{
     * case class Box[T](contents: T)
     * given Liftable[Box] with
     *   def map[A, B](fa: Box[A])(f: A => B) = Box(f(fa.contents))
     *
     * val result = LinearFn.apply((ex1, ex2))(refs =>
     *   (Box(refs._1).lift, refs._2)  // .lift normalizes the type
     * )
     * }}}
     */
    extension [F[_], A, D <: Tuple](container: F[Restricted[A, D]])(using ev: Liftable[F])
      def lift: Restricted[F[A], D] =
        LinearRef[F[A], D](() => ev.map(container)(_.execute()))

  // Implement abstract methods from LinearFnBase
  protected def makeLinearRef[A, D <: Tuple](fn: () => A): Restricted[A, D] =
    Restricted.LinearRef(fn)

  protected def executeRestricted[A, D <: Tuple](r: Restricted[A, D]): A =
    r.execute()