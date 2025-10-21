package linearfn

import scala.language.implicitConversions
import scala.language.dynamics
import NamedTuple.{AnyNamedTuple, NamedTuple}
import scala.annotation.{implicitNotFound, targetName}
import scala.quoted.*
import scala.compiletime.summonInline
import Utils.*

/**
 * Use Dynamic but with macros for type safety.
 */
object RestrictedDynamicMacros:
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

  // Extract the B type parameters from a tuple of Restricted
  type ExtractResultTypes[RQT <: Tuple] <: Tuple = RQT match
    case EmptyTuple => EmptyTuple
    case Restricted[a, _] *: tail => a *: ExtractResultTypes[tail]

  // Extract the D type parameters from a tuple of Restricted
  type ExtractDependencyTypes[RQT <: Tuple] <: Tuple = RQT match
    case EmptyTuple => EmptyTuple
    case Restricted[_, d] *: tail => d *: ExtractDependencyTypes[tail]

  type Collate[A, D <: Tuple] <: Tuple = A match
    case Restricted[_, d] =>
      Tuple.Concat[d, D]
    case _ =>
      D

  type CollateAll[A <: Tuple, D <: Tuple] <: Tuple = A match
    case EmptyTuple => D
    case h *: t => CollateAll[t, Collate[h, D]]

  def tupleExecute[T <: Tuple](t: T): Tuple =
    t match
      case EmptyTuple => EmptyTuple
      case (h: Restricted[_, _]) *: tail =>
        h.execute() *:
          tupleExecute(tail)

  trait Restricted[A, D <: Tuple] extends Dynamic:
    // Macro-based implementations for compile-time verification
    transparent inline def selectDynamic(inline name: String): Any =
      ${ RestrictedMacros.selectDynamicImpl[A, D]('this, 'name) }

    transparent inline def applyDynamic(inline name: String)(inline args: Any*): Any =
      ${ RestrictedMacros.applyDynamicImpl[A, D]('this, 'name, 'args) }

    def execute(): A

  object Restricted:
    case class LinearRef[A, D <: Tuple](val fn: () => A) extends Restricted[A, D]:
      def execute(): A = fn()

  object LinearFn:
    // AT: argument tuple, RQT: restricted return tuple
    // RT and DT are computed from RQT
    def apply[AT <: Tuple, RQT <: Tuple]
    (args: AT)
    (fns: ToLinearRef[AT] => RQT)
    (using @implicitNotFound("Number of actual arguments must match the number of elements returned by fns")
    ev0: Tuple.Size[AT] =:= Tuple.Size[RQT])
//        /* DEBUG */ (using @implicitNotFound("DEBUG: RQT = ${RQT}") debugRQT: RQT =:= Nothing)
    (using @implicitNotFound("Cannot extract dependencies, is the query affine?")
    ev2: ExtractDependencyTypes[RQT] <:< InverseMapDeps[RQT])
    (using @implicitNotFound("Failed to match restricted types")
    ev3: RQT =:= ToRestricted[ExtractResultTypes[RQT], ExtractDependencyTypes[RQT]])
    (using @implicitNotFound("Recursive definitions must be linear")
    ev4: ExpectedResult[AT] <:< ActualResult[RQT]) =
      val argsRefs = args.toArray.map(a => Restricted.LinearRef(() => a))
      val refsTuple = Tuple.fromArray(argsRefs).asInstanceOf[ToLinearRef[AT]]
      val exec = fns(refsTuple)
      println(exec)
      tupleExecute(exec)