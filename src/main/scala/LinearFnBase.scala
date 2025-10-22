package linearfn

import Utils.*
import scala.annotation.implicitNotFound

/**
 * Abstract base for all LinearFn implementations.
 * Contains all the common match types and apply logic.
 * Subclasses only need to provide:
 * - Restricted trait implementation
 * - LinearRef case class implementation
 */
abstract class LinearFnBase:

  // Abstract types to be defined by implementations
  type Restricted[A, D <: Tuple]

  // Factory method for creating LinearRef instances
  protected def makeLinearRef[A, D <: Tuple](fn: () => A): Restricted[A, D]

  // Execute a Restricted value
  protected def executeRestricted[A, D <: Tuple](r: Restricted[A, D]): A

  // Common match types
  type ToLinearRef[AT <: Tuple] = Tuple.Map[ZipWithIndex[AT], [T] =>> T match
    case (elem, index) => Restricted[elem, Tuple1[index]]]

  type InverseMapDeps[RT <: Tuple] <: Tuple = RT match {
    case Restricted[_, d] *: t => HasDuplicate[d] *: InverseMapDeps[t]
    // Automatic lifting: handle nested containers
    case List[Restricted[_, d]] *: t => HasDuplicate[d] *: InverseMapDeps[t]
    case Option[Restricted[_, d]] *: t => HasDuplicate[d] *: InverseMapDeps[t]
    case Vector[Restricted[_, d]] *: t => HasDuplicate[d] *: InverseMapDeps[t]
    case EmptyTuple => EmptyTuple
  }

  type ToRestricted[AT <: Tuple, DT <: Tuple] =
    Tuple.Map[Tuple.Zip[AT, DT], [T] =>> ConstructRestricted[T]]

  type ConstructRestricted[T] = T match
    // Automatic lifting: construct containers of Restricted types (must come BEFORE general case)
    case (List[a], d) => List[Restricted[a, d]]
    case (Option[a], d) => Option[Restricted[a, d]]
    case (Vector[a], d) => Vector[Restricted[a, d]]
    case (a, d) => Restricted[a, d]

  type ExtractDependencies[D] <: Tuple = D match
    case Restricted[_, d] => d
    // Automatic lifting: extract dependencies from containers
    case List[Restricted[_, d]] => d
    case Option[Restricted[_, d]] => d
    case Vector[Restricted[_, d]] => d

  type ExpectedResult[QT <: Tuple] = Tuple.Union[GenerateIndices[0, Tuple.Size[QT]]]
  type ActualResult[RT <: Tuple] = Tuple.Union[Tuple.FlatMap[RT, ExtractDependencies]]

  type ExtractResultTypes[RQT <: Tuple] <: Tuple = RQT match
    case EmptyTuple => EmptyTuple
    case Restricted[a, d] *: tail => a *: ExtractResultTypes[tail]
    // Automatic lifting: treat T[Restricted[a, d]] as Restricted[T[a], d]
    case List[Restricted[a, d]] *: tail => List[a] *: ExtractResultTypes[tail]
    case Option[Restricted[a, d]] *: tail => Option[a] *: ExtractResultTypes[tail]
    case Vector[Restricted[a, d]] *: tail => Vector[a] *: ExtractResultTypes[tail]

  type ExtractDependencyTypes[RQT <: Tuple] <: Tuple = RQT match
    case EmptyTuple => EmptyTuple
    case Restricted[a, d] *: tail => d *: ExtractDependencyTypes[tail]
    // Automatic lifting: extract dependencies from T[Restricted[a, d]]
    case List[Restricted[a, d]] *: tail => d *: ExtractDependencyTypes[tail]
    case Option[Restricted[a, d]] *: tail => d *: ExtractDependencyTypes[tail]
    case Vector[Restricted[a, d]] *: tail => d *: ExtractDependencyTypes[tail]

  type CollateDeps[A, D <: Tuple] <: Tuple = A match
    case Restricted[a, d] => Tuple.Concat[d, D]
    case _ => D

  type CollateAllDeps[A <: Tuple, D <: Tuple] <: Tuple = A match
    case EmptyTuple => D
    case h *: t => CollateAllDeps[t, CollateDeps[h, D]]

  // Common tupleExecute implementation
  def tupleExecute[T <: Tuple](t: T): Tuple =
    t match
      case EmptyTuple => EmptyTuple
      // Automatic lifting: execute nested containers (must come BEFORE Restricted case)
      case (h: List[_]) *: tail =>
        val list = h.asInstanceOf[List[Any]]
        val executed = list.map {
          case r: Restricted[_, _] => executeRestricted(r)
          case other => other
        }
        executed *: tupleExecute(tail)
      case (h: Option[_]) *: tail =>
        val opt = h.asInstanceOf[Option[Any]]
        val executed = opt.map {
          case r: Restricted[_, _] => executeRestricted(r)
          case other => other
        }
        executed *: tupleExecute(tail)
      case (h: Vector[_]) *: tail =>
        val vec = h.asInstanceOf[Vector[Any]]
        val executed = vec.map {
          case r: Restricted[_, _] => executeRestricted(r)
          case other => other
        }
        executed *: tupleExecute(tail)
      case (h: Restricted[_, _]) *: tail =>
        executeRestricted(h) *: tupleExecute(tail)

  // Common LinearFn.apply implementation
  object LinearFn:
    def apply[AT <: Tuple, DT <: Tuple, RT <: Tuple, RQT <: Tuple]
    (args: AT)
    (fns: ToLinearRef[AT] => RQT)
    (using @implicitNotFound("Cannot extract result types from RQT") ev1: RT =:= ExtractResultTypes[RQT])
//        /* DEBUG */ (using @implicitNotFound("DEBUG: RQT = ${RQT}") debugRQT: RQT =:= Nothing)
    (using @implicitNotFound("Cannot extract dependencies from RQT") ev1b: DT =:= ExtractDependencyTypes[RQT])
    (using @implicitNotFound("Number of actual arguments must match the number of elements returned by fns") ev0: Tuple.Size[AT] =:= Tuple.Size[RT])
    (using @implicitNotFound("Cannot extract dependencies, is the query affine?") ev2: InverseMapDeps[RQT] =:= DT)
    (using @implicitNotFound("Failed to match restricted types: ${RQT}") ev3: RQT =:= ToRestricted[RT, DT])
    (using @implicitNotFound("Recursive definitions must be linear: ${RT}") ev4: ExpectedResult[AT] <:< ActualResult[RQT]): RT =
      val argsRefs = args.toArray.map(a => makeLinearRef(() => a))
      val refsTuple = Tuple.fromArray(argsRefs).asInstanceOf[ToLinearRef[AT]]
      val exec = fns(refsTuple)
      println(exec)
      tupleExecute(exec).asInstanceOf[RT]
