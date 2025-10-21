package linearfn

import Utils.*
import scala.language.implicitConversions
import scala.language.dynamics
import NamedTuple.{AnyNamedTuple, NamedTuple}
import scala.annotation.implicitNotFound

/**
 * Use Dynamic to proxy field/method access.
 * Not good because not type safe, but works with any type.
 * TODO: stage computation such that type checking happens on fn?
 */
object RestrictedDynamic:

  type ToLinearRef[AT <: Tuple] = Tuple.Map[ZipWithIndex[AT], [T] =>> T match
    case (elem, index) => Restricted[elem, elem, Tuple1[index]]]

  type InverseMapDeps[RT <: Tuple] <: Tuple = RT match {
    case Restricted[a, b, d] *: t => HasDuplicate[d] *: InverseMapDeps[t]
    case EmptyTuple => EmptyTuple
  }

  type ToRestricted[AT <: Tuple, DT <: Tuple] =
    Tuple.Map[Tuple.Zip[AT, DT], [T] =>> ConstructRestricted[T]]

  type ConstructRestricted[T] = T match
    case (a, d) => Restricted[a, a, d]

  type ExtractDependencies[D] <: Tuple = D match
    case Restricted[a, b, d] => d

  type ExpectedResult[QT <: Tuple] = Tuple.Union[GenerateIndices[0, Tuple.Size[QT]]]
  type ActualResult[RT <: Tuple] = Tuple.Union[Tuple.FlatMap[RT, ExtractDependencies]]

  type StripRestricted[T] = T match
    case Restricted[a, b, d] => b

  type ExtractResultTypes[RQT <: Tuple] <: Tuple = RQT match
    case EmptyTuple => EmptyTuple
    case Restricted[a, b, d] *: tail => b *: ExtractResultTypes[tail]
  type ExtractDependencyTypes[RQT <: Tuple] <: Tuple = RQT match
    case EmptyTuple => EmptyTuple
    case Restricted[a, b, d] *: tail => d *: ExtractDependencyTypes[tail]

  def tupleExecute[T <: Tuple](t: T): Tuple =
    t match
      case EmptyTuple => EmptyTuple
      case (h: Restricted[_, _, _]) *: tail =>
        h.execute() *:
          tupleExecute(tail)

  trait Restricted[A, B, D <: Tuple] extends Dynamic:
    def selectDynamic(name: String): Restricted[?, ?, ?] =
      println(s"field access $name")
      ???

    def applyDynamic(name: String)(args: Any*): Restricted[?, ?, ?] = {
      println(s"applying $name with args: $args")
      ???
    }

    def get: A

    def execute(): B

  object Restricted:
    case class LinearRef[A, B, D <: Tuple](protected val wrapped: A, protected val fn: A => B) extends Restricted[A, B, D]:
      def get: A = wrapped

      def execute(): B = fn(wrapped)

  object LinearFn:
    def apply[AT <: Tuple, DT <: Tuple, RT <: Tuple, RQT <: Tuple]
    (args: AT)
    (fns: ToLinearRef[AT] => RQT)
    (using @implicitNotFound("Cannot extract result types from RQT") ev1: RT =:= ExtractResultTypes[RQT])
    (using @implicitNotFound("Number of actual arguments must match the number of elements returned by fns") ev0: Tuple.Size[AT] =:= Tuple.Size[RT])
    (using @implicitNotFound("Cannot extract dependencies, is the query affine?") ev2: DT <:< InverseMapDeps[RQT])
//    (using @implicitNotFound("Failed: ${RQT}") ev3: RT <:< ToRestricted[RT, DT])
    (using @implicitNotFound("Failed to match restricted types")
      ev3: RQT =:= ToRestricted[RT, ExtractDependencyTypes[RQT]])
    (using @implicitNotFound("Recursive definitions must be linear: ${RT}") ev4: ExpectedResult[AT] <:< ActualResult[RQT]) =
      val argsRefs = args.toArray.map(a => Restricted.LinearRef(Some(a), x => x))
      val refsTuple = Tuple.fromArray(argsRefs).asInstanceOf[ToLinearRef[AT]]
      val exec = fns(refsTuple)
      println(exec)
      tupleExecute(exec)
