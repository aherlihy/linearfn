package linearfn

import Utils.*
import scala.language.implicitConversions
import scala.language.dynamics
import NamedTuple.{AnyNamedTuple, NamedTuple}
import scala.annotation.implicitNotFound

/**
 * Use Dynamic to proxy field/method access.
 * Not ideal because not type safe, but unlike Selectable works with any input type and applyDynamic is called.
 */
object RestrictedDynamic extends LinearFnBase:

  // Implementation-specific Restricted trait
  trait Restricted[A, D <: Tuple, C <: Tuple] extends Dynamic:
    def stageField(name: String): Restricted[A, D, C]
    def stageCall[D2 <: Tuple, C2 <: Tuple](name: String, args: Tuple): Restricted[A, D2, C2]

    def selectDynamic(name: String): Restricted[A, D, C] = {
      println(s"field access $name")
      stageField(name)
    }

    def applyDynamic[T1](method: String)(arg: T1): Restricted[A, CollateDeps[T1, D], C] = {
      println(s"applying $method with arg: $arg")
      stageCall(method, Tuple1(arg))
    }

    // TODO: compiler says you can't overload except for different # arguments, then gives example that is exactly this, still fails tho:
    //  def applyDynamic[T1, T2](method: String)(arg1: T1, arg2: T2): Restricted[A, B, CollateAll[(T1, T2), D]] = {
    //    println(s"applying $method with args: $arg1, $arg2")
    //    stageCall(method, (arg1, arg2))
    //  }

    def execute(): A

  // Implementation-specific LinearRef
  object Restricted:
    case class LinearRef[A, D <: Tuple, C <: Tuple](protected val fn: () => A) extends Restricted[A, D, C]:
      def execute(): A = fn()

      override def stageField(name: String): Restricted[A, D, C] =
        LinearRef(() =>
          println(s"inside fn: staged field access $name")
          // should be equivalent to fn().name
          ???
        )

      override def stageCall[D2 <: Tuple, C2 <: Tuple](name: String, args: Tuple): Restricted[A, D2, C2] = {
        println(s"staging call $name with args: $args")
        LinearRef[A, D2, C2](() =>
          println(s"inside fn: staged call $name with args: $args")
          // should be equivalent to fn().name(args)
          ???
        )
      }

  // Implement abstract methods from LinearFnBase
  protected def makeLinearRef[A, D <: Tuple, C <: Tuple](fn: () => A): Restricted[A, D, C] =
    Restricted.LinearRef(fn)

  protected def executeRestricted[A, D <: Tuple, C <: Tuple](r: Restricted[A, D, C]): A =
    r.execute()
