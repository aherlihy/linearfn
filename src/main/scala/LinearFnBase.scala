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

  // Helper match type to extract inner type and dependencies from potentially nested containers
  // Returns (innermost_type, dependencies)
  // This is the PRIMARY place to add new container types for lifting support
  type LiftInnerType[T] = T match
    case Restricted[a, d] => (a, d)
    case List[inner] => LiftInnerType[inner] match
      case (a, d) => (List[a], d)
    case Option[inner] => LiftInnerType[inner] match
      case (a, d) => (Option[a], d)
    case Vector[inner] => LiftInnerType[inner] match
      case (a, d) => (Vector[a], d)

  // Common match types
  type ToLinearRef[AT <: Tuple] = Tuple.Map[ZipWithIndex[AT], [T] =>> T match
    case (elem, index) => Restricted[elem, Tuple1[index]]]

  type InverseMapDeps[RT <: Tuple] <: Tuple = RT match {
    case EmptyTuple => EmptyTuple
    case h *: t => LiftInnerType[h] match
      case (_, d) => HasDuplicate[d] *: InverseMapDeps[t]
  }

  type ToRestricted[AT <: Tuple, DT <: Tuple] =
    Tuple.Map[Tuple.Zip[AT, DT], [T] =>> ConstructRestricted[T]]

  // Recursive helper to reconstruct nested Restricted types
  type ReconstructRestricted[A, D <: Tuple] = A match
    case List[inner] => List[ReconstructRestricted[inner, D]]
    case Option[inner] => Option[ReconstructRestricted[inner, D]]
    case Vector[inner] => Vector[ReconstructRestricted[inner, D]]
    case _ => Restricted[A, D]

  type ConstructRestricted[T] = T match
    // Automatic lifting: construct containers of Restricted types (must come BEFORE general case)
    case (List[a], d) => ReconstructRestricted[List[a], d]
    case (Option[a], d) => ReconstructRestricted[Option[a], d]
    case (Vector[a], d) => ReconstructRestricted[Vector[a], d]
    case (a, d) => Restricted[a, d]

  // Recursively extract dependencies from nested containers
  // Note: This uses a different recursion pattern than LiftInnerType
  type ExtractDependencies[D] <: Tuple = D match
    case Restricted[_, d] => d
    case List[inner] => ExtractDependencies[inner]
    case Option[inner] => ExtractDependencies[inner]
    case Vector[inner] => ExtractDependencies[inner]

  type ExpectedResult[QT <: Tuple] = Tuple.Union[GenerateIndices[0, Tuple.Size[QT]]]
  type ActualResult[RT <: Tuple] = Tuple.Union[Tuple.FlatMap[RT, ExtractDependencies]]

  type ExtractResultTypes[RQT <: Tuple] <: Tuple = RQT match
    case EmptyTuple => EmptyTuple
    case h *: tail => LiftInnerType[h] match
      case (a, _) => a *: ExtractResultTypes[tail]

  type ExtractDependencyTypes[RQT <: Tuple] <: Tuple = RQT match
    case EmptyTuple => EmptyTuple
    case h *: tail => LiftInnerType[h] match
      case (_, d) => d *: ExtractDependencyTypes[tail]

  type CollateDeps[A, D <: Tuple] <: Tuple = A match
    case Restricted[a, d] => Tuple.Concat[d, D]
    case _ => D

  type CollateAllDeps[A <: Tuple, D <: Tuple] <: Tuple = A match
    case EmptyTuple => D
    case h *: t => CollateAllDeps[t, CollateDeps[h, D]]

  // Helper to recursively execute Restricted values inside nested containers
  private def executeNested(value: Any): Any = value match
    // Check for containers BEFORE Restricted to handle nested cases
    case list: List[_] => list.map(executeNested)
    case opt: Option[_] => opt.map(executeNested)
    case vec: Vector[_] => vec.map(executeNested)
    case r: Restricted[_, _] => executeRestricted(r)
    case other => other

  // Common tupleExecute implementation
  def tupleExecute[T <: Tuple](t: T): Tuple =
    t match
      case EmptyTuple => EmptyTuple
      case h *: tail => executeNested(h) *: tupleExecute(tail)

  // Common LinearFn.apply implementation
  object LinearFn:
    def apply[AT <: Tuple, DT <: Tuple, RT <: Tuple, RQT <: Tuple]
    (args: AT)
    (fns: ToLinearRef[AT] => RQT)
    (using @implicitNotFound("Cannot extract result types from RQT") ev1: RT =:= ExtractResultTypes[RQT])
//        /* DEBUG */ (using @implicitNotFound("DEBUG: RQT = ${RQT}") debugRQT: RQT =:= Nothing)
    (using @implicitNotFound("Cannot extract dependencies from RQT") ev1b: DT =:= ExtractDependencyTypes[RQT])
    (using @implicitNotFound("Linear functions must have the same number of argument and return types and the return types must be Restricted") ev0: Tuple.Size[AT] =:= Tuple.Size[RT])
    (using @implicitNotFound("Cannot extract dependencies, is the query affine?") ev2: InverseMapDeps[RQT] =:= DT)
    (using @implicitNotFound("Failed to match restricted types: ${RQT}") ev3: RQT =:= ToRestricted[RT, DT])
    (using @implicitNotFound("Recursive definitions must be linear: ${RT}") ev4: ExpectedResult[AT] <:< ActualResult[RQT]): RT =
      val argsRefs = args.toArray.map(a => makeLinearRef(() => a))
      val refsTuple = Tuple.fromArray(argsRefs).asInstanceOf[ToLinearRef[AT]]
      val exec = fns(refsTuple)
      println(exec)
      tupleExecute(exec).asInstanceOf[RT]
