package linearfn

import Utils.*
import scala.language.implicitConversions
import scala.language.reflectiveCalls
import NamedTuple.{AnyNamedTuple, NamedTuple}
import scala.annotation.implicitNotFound

/**
 * Use Selectable to proxy field/method access.
 * TODO: applyDynamic doesn't work yet, needs return type?
 * Good because type safe, but doesn't work out of the box with non-product types.
 * TODO: add an implicit conversion wrapper from Int to something that can use Fields?
 * Ideally should be able to work with any type.
 */
object RestrictedSelectable:

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

  def tupleExecute[T <: Tuple](t: T): Tuple =
    t match
      case EmptyTuple => EmptyTuple
      case (h: Restricted[_, _, _]) *: tail =>
        h.execute() *:
          tupleExecute(tail)

  trait Restricted[A, B, D <: Tuple] extends Selectable:
    type Fields = NamedTuple.Map[NamedTuple.From[B], [T] =>> Restricted[A, T, D]]
    def selectDynamic(name: String) =
      println(s"field access $name")
      ???
    // TODO: how to get return type of name on B and wrap in Restricted?
    def applyDynamic(name: String, ctags: Class[?]*)(args: Any*) =
      println(s"applying $name with args: $args")
      ???

    def get: A
    def execute(): B
  object Restricted:
    case class LinearRef[A, B, D <: Tuple](protected val wrapped: A, protected val fn: A => B) extends Restricted[A, B, D]:
      def get: A = wrapped
      def execute(): B = fn(wrapped)

  object LinearFn:
    def apply[AT <: Tuple, DT <: Tuple, RT <: Tuple]
    (args: AT)
    (fns: ToLinearRef[AT] => RT)
    (using @implicitNotFound("Number of actual arguments must match the number of elements returned by fns") ev0: Tuple.Size[AT] =:= Tuple.Size[RT])
    (using @implicitNotFound("Cannot extract dependencies, is the query affine?") ev2: DT <:< InverseMapDeps[RT])
    (using @implicitNotFound("Failed: ${RQT}") ev3: RT <:< ToRestricted[AT, DT])
    (using @implicitNotFound("Recursive definitions must be linear: ${RT}") ev4: ExpectedResult[AT] <:< ActualResult[RT]) =
      val argsRefs = args.toArray.map(a => Restricted.LinearRef(Some(a), x => x))
      val refsTuple = Tuple.fromArray(argsRefs).asInstanceOf[ToLinearRef[AT]]
      val exec = fns(refsTuple)
      println(exec)
      tupleExecute(exec)