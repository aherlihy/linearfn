package linearfn

import Utils.*
import scala.language.implicitConversions
import scala.language.reflectiveCalls
import NamedTuple.{AnyNamedTuple, NamedTuple}
import scala.annotation.implicitNotFound
import scala.reflect.Selectable.reflectiveSelectable

/**
 * Use Selectable to proxy field/method access.
 * TODO: applyDynamic doesn't work yet, needs return type?
 * Good because type safe, but doesn't work out of the box with non-product types.
 * TODO: add an implicit conversion wrapper from Int to something that can use Fields?
 * Ideally should be able to work with any type.
 */
object RestrictedSelectable:

  type ToLinearRef[AT <: Tuple] = Tuple.Map[ZipWithIndex[AT], [T] =>> T match
    case (elem, index) => Restricted[elem, Tuple1[index]]]

  type InverseMapDeps[RT <: Tuple] <: Tuple = RT match {
    case Restricted[_, d] *: t => HasDuplicate[d] *: InverseMapDeps[t]
    case EmptyTuple => EmptyTuple
  }

  type ToRestricted[AT <: Tuple, DT <: Tuple] =
    Tuple.Map[Tuple.Zip[AT, DT], [T] =>> ConstructRestricted[T]]

  type ConstructRestricted[T] = T match
    case (a, d) => Restricted[a, d]

  type ExtractDependencies[D] <: Tuple = D match
    case Restricted[_, d] => d

  type ExpectedResult[QT <: Tuple] = Tuple.Union[GenerateIndices[0, Tuple.Size[QT]]]
  type ActualResult[RT <: Tuple] = Tuple.Union[Tuple.FlatMap[RT, ExtractDependencies]]

  type ExtractResultTypes[RQT <: Tuple] <: Tuple = RQT match
    case EmptyTuple => EmptyTuple
    case Restricted[a, d] *: tail => a *: ExtractResultTypes[tail]
  type ExtractDependencyTypes[RQT <: Tuple] <: Tuple = RQT match
    case EmptyTuple => EmptyTuple
    case Restricted[a, d] *: tail => d *: ExtractDependencyTypes[tail]

  type CollateDeps[A, D <: Tuple] <: Tuple = A match
    case Restricted[a, d] =>
      Tuple.Concat[d, D]
    case _ =>
      D

  type CollateAllDeps[A <: Tuple, D <: Tuple] <: Tuple = A match
    case EmptyTuple => D
    case h *: t => CollateAllDeps[t, CollateDeps[h, D]]

  def tupleExecute[T <: Tuple](t: T): Tuple =
    t match
      case EmptyTuple => EmptyTuple
      case (h: Restricted[_, __]) *: tail =>
        h.execute() *:
          tupleExecute(tail)

  trait Restricted[A, D <: Tuple] extends Selectable:
    type Fields = NamedTuple.Map[NamedTuple.From[A], [T] =>> Restricted[T, D]]
    def stageField(name: String): Restricted[A, D]
    def stageCall[D2 <: Tuple](name: String, args: Tuple): Restricted[A, D2]

    def selectDynamic(name: String) = {
      println(s"field access $name")
      stageField(name)
    }

    def applyDynamic[T1](method: String)(arg: T1): Restricted[A, CollateDeps[T1, D]] = {
      println(s"applying $method with arg: $arg")
      stageCall(method, Tuple1(arg))
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

      override def stageCall[D2 <: Tuple](name: String, args: Tuple): Restricted[A, D2] = {
        println(s"staging call $name with args: $args")
        LinearRef[A, D2](() =>
          println(s"inside fn: staged call $name with args: $args")
          // should be equivalent to fn().name(args)
          ???
        )
      }

  object LinearFn:
    def apply[AT <: Tuple, DT <: Tuple, RT <: Tuple, RQT <: Tuple]
    (args: AT)
    (fns: ToLinearRef[AT] => RQT)
    (using @implicitNotFound("Cannot extract result types from RQT") ev1: RT =:= ExtractResultTypes[RQT])
    (using @implicitNotFound("Cannot extract dependencies from RQT") ev1b: DT =:= ExtractDependencyTypes[RQT])
    (using @implicitNotFound("Number of actual arguments must match the number of elements returned by fns") ev0: Tuple.Size[AT] =:= Tuple.Size[RT])
    (using @implicitNotFound("Cannot extract dependencies, is the query affine?") ev2: InverseMapDeps[RQT] =:= DT)
    (using @implicitNotFound("Failed to match restricted types: ${RQT}") ev3: RQT =:= ToRestricted[RT, DT])
    (using @implicitNotFound("Recursive definitions must be linear: ${RT}") ev4: ExpectedResult[AT] <:< ActualResult[RQT]) =
      val argsRefs = args.toArray.map(a => Restricted.LinearRef(() => a))
      val refsTuple = Tuple.fromArray(argsRefs).asInstanceOf[ToLinearRef[AT]]
      val exec = fns(refsTuple)
      println(exec)
      tupleExecute(exec)