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
// Base trait that all Restricted implementations must extend
// This allows runtime type checking in the abstract base class
trait RestrictedBase[A, D <: Tuple, C <: Tuple]:
  def execute(): A

abstract class LinearFnBase:

  // Abstract type with upper bound constraint
  // All implementations must extend RestrictedBase
  type Restricted[A, D <: Tuple, C <: Tuple] <: RestrictedBase[A, D, C]

  // Factory method for creating LinearRef instances
  protected def makeLinearRef[A, D <: Tuple, C <: Tuple](fn: () => A): Restricted[A, D, C]

  // Execute a Restricted value
  protected def executeRestricted[A, D <: Tuple, C <: Tuple](r: Restricted[A, D, C]): A =
    r.execute()

  // Helper match type to extract inner type, dependencies, and consumed state from potentially nested containers
  // Returns (innermost_type, dependencies, consumption_state)
  // This is the PRIMARY place to add new container types for lifting support
  type LiftInnerType[T] = T match
    case Restricted[a, d, c] => (a, d, c)
    case List[inner] => LiftInnerType[inner] match
      case (a, d, c) => (List[a], d, c)
    case Option[inner] => LiftInnerType[inner] match
      case (a, d, c) => (Option[a], d, c)
    case Vector[inner] => LiftInnerType[inner] match
      case (a, d, c) => (Vector[a], d, c)

  // Common match types
  type ToLinearRef[AT <: Tuple] = Tuple.Map[ZipWithIndex[AT], [T] =>> T match
    case (elem, index) => Restricted[elem, Tuple1[index], EmptyTuple]
  ]

  type InverseMapDeps[RT <: Tuple] <: Tuple = RT match {
    case EmptyTuple => EmptyTuple
    case h *: t => LiftInnerType[h] match
      case (_, d, _) => HasDuplicate[d] *: InverseMapDeps[t]
  }

  type ToRestricted[AT <: Tuple, DT <: Tuple, CT <: Tuple] =
    Tuple.Map[Tuple.Zip[Tuple.Zip[AT, DT], CT], [T] =>> ConstructRestricted[T]]

  // Recursive helper to reconstruct nested Restricted types
  type ReconstructRestricted[A, D <: Tuple, C <: Tuple] = A match
    case List[inner] => List[ReconstructRestricted[inner, D, C]]
    case Option[inner] => Option[ReconstructRestricted[inner, D, C]]
    case Vector[inner] => Vector[ReconstructRestricted[inner, D, C]]
    case _ => Restricted[A, D, C]

  type ConstructRestricted[T] = T match
    // Automatic lifting: construct containers of Restricted types (must come BEFORE general case)
    case ((List[a], d), c) => ReconstructRestricted[List[a], d, c]
    case ((Option[a], d), c) => ReconstructRestricted[Option[a], d, c]
    case ((Vector[a], d), c) => ReconstructRestricted[Vector[a], d, c]
    case ((a, d), c) => Restricted[a, d, c]

  // Recursively extract dependencies from nested containers
  // Note: This uses a different recursion pattern than LiftInnerType
  type ExtractDependencies[D] <: Tuple = D match
    case Restricted[_, d, _] => d
    case List[inner] => ExtractDependencies[inner]
    case Option[inner] => ExtractDependencies[inner]
    case Vector[inner] => ExtractDependencies[inner]

  type ExpectedResult[QT <: Tuple] = Tuple.Union[GenerateIndices[0, Tuple.Size[QT]]]
  type ActualResult[RT <: Tuple] = Tuple.Union[Tuple.FlatMap[RT, ExtractDependencies]]

  type ExtractResultTypes[RQT <: Tuple] <: Tuple = RQT match
    case EmptyTuple => EmptyTuple
    case h *: tail => LiftInnerType[h] match
      case (a, _, _) => a *: ExtractResultTypes[tail]

  type ExtractDependencyTypes[RQT <: Tuple] <: Tuple = RQT match
    case EmptyTuple => EmptyTuple
    case h *: tail => LiftInnerType[h] match
      case (_, d, _) => d *: ExtractDependencyTypes[tail]

  type ExtractConsumedTypes[RQT <: Tuple] <: Tuple = RQT match
    case EmptyTuple => EmptyTuple
    case h *: tail => LiftInnerType[h] match
      case (_, _, c) => c *: ExtractConsumedTypes[tail]

  type CollateDeps[A, D <: Tuple] <: Tuple = A match
    case Restricted[a, d, _] => Tuple.Concat[d, D]
    case _ => D
  type CollateConsumed[A, C <: Tuple] <: Tuple = A match
    case Restricted[a, _, c] => Tuple.Concat[c, C]
    case _ => C

  type CollateAllDeps[A <: Tuple, D <: Tuple] <: Tuple = A match
    case EmptyTuple => D
    case h *: t => CollateAllDeps[t, CollateDeps[h, D]]

  // Helper to recursively execute Restricted values inside nested containers
  // We can check against RestrictedBase since all Restricted types must extend it
  private def executeNested(value: Any): Any = value match
    // Check for containers BEFORE Restricted to handle nested cases
    case list: List[_] => list.map(executeNested)
    case opt: Option[_] => opt.map(executeNested)
    case vec: Vector[_] => vec.map(executeNested)
    case r: RestrictedBase[_, _, _] => r.execute()
    case other => other

  // Common tupleExecute implementation
  def tupleExecute[T <: Tuple](t: T): Tuple =
    t match
      case EmptyTuple => EmptyTuple
      case h *: tail => executeNested(h) *: tupleExecute(tail)

  // Helper to check that all consumption state tuples have length 0 or 1
  type AllConsumedStatesAtMostOne[CT <: Tuple] <: Boolean = CT match
    case EmptyTuple => true
    case h *: t => Tuple.Size[h] match
      case 0 => AllConsumedStatesAtMostOne[t]
      case 1 => AllConsumedStatesAtMostOne[t]
      case _ => false

  // Helper to check that all consumption state tuples have exactly length 1
  type AllConsumedStatesExactlyOne[CT <: Tuple] <: Boolean = CT match
    case EmptyTuple => true
    case h *: t => Tuple.Size[h] match
      case 1 => AllConsumedStatesExactlyOne[t]
      case _ => false

  // Common LinearFn.apply implementation
  object LinearFn:
    def apply[AT <: Tuple, DT <: Tuple, CT <: Tuple, RT <: Tuple, RQT <: Tuple]
    (args: AT)
    (fns: ToLinearRef[AT] => RQT)
    (using @implicitNotFound("Cannot extract result types from RQT") ev1: RT =:= ExtractResultTypes[RQT])
//        /* DEBUG */ (using @implicitNotFound("DEBUG: RQT = ${RQT}") debugRQT: RQT =:= Nothing)
    (using @implicitNotFound("Cannot extract dependencies from RQT") ev1b: DT =:= ExtractDependencyTypes[RQT])
    (using @implicitNotFound("Cannot extract consumption states from RQT") ev1c: CT =:= ExtractConsumedTypes[RQT])
    (using @implicitNotFound("Linear functions must have the same number of argument and return types and the return types must be Restricted") ev0: Tuple.Size[AT] =:= Tuple.Size[RT])
    (using @implicitNotFound("Cannot extract dependencies, is the query affine?") ev2: InverseMapDeps[RQT] =:= DT)
    (using @implicitNotFound("Failed to match restricted types: ${RQT}") ev3: RQT =:= ToRestricted[RT, DT, CT])
    (using @implicitNotFound("Recursive definitions must be linear: ${RT}") ev4: ExpectedResult[AT] <:< ActualResult[RQT])
    (using @implicitNotFound("All return values must have consumption state of length 0 or 1") ev5: AllConsumedStatesAtMostOne[CT] =:= true): RT =
      val argsRefs = args.toArray.map(a => makeLinearRef(() => a))
      val refsTuple = Tuple.fromArray(argsRefs).asInstanceOf[ToLinearRef[AT]]
      val exec = fns(refsTuple)
      println(exec)
      tupleExecute(exec).asInstanceOf[RT]

    /**
     * applyConsumed - variant of apply that requires all return values to be consumed.
     * Use this when you want to ensure that all results have been consumed exactly once.
     */
    def applyConsumed[AT <: Tuple, DT <: Tuple, CT <: Tuple, RT <: Tuple, RQT <: Tuple]
    (args: AT)
    (fns: ToLinearRef[AT] => RQT)
    (using @implicitNotFound("Cannot extract result types from RQT") ev1: RT =:= ExtractResultTypes[RQT])
    (using @implicitNotFound("Cannot extract dependencies from RQT") ev1b: DT =:= ExtractDependencyTypes[RQT])
    (using @implicitNotFound("Cannot extract consumption states from RQT") ev1c: CT =:= ExtractConsumedTypes[RQT])
    (using @implicitNotFound("Linear functions must have the same number of argument and return types and the return types must be Restricted") ev0: Tuple.Size[AT] =:= Tuple.Size[RT])
    (using @implicitNotFound("Cannot extract dependencies, is the query affine?") ev2: InverseMapDeps[RQT] =:= DT)
    (using @implicitNotFound("Failed to match restricted types: ${RQT}") ev3: RQT =:= ToRestricted[RT, DT, CT])
    (using @implicitNotFound("Recursive definitions must be linear: ${RT}") ev4: ExpectedResult[AT] <:< ActualResult[RQT])
    (using @implicitNotFound("All return values must be consumed (consumption state length must be exactly 1)") ev5: AllConsumedStatesExactlyOne[CT] =:= true): RT =
      val argsRefs = args.toArray.map(a => makeLinearRef(() => a))
      val refsTuple = Tuple.fromArray(argsRefs).asInstanceOf[ToLinearRef[AT]]
      val exec = fns(refsTuple)
      println(exec)
      tupleExecute(exec).asInstanceOf[RT]

    /**
     * applyMulti - allows arbitrary number of return values (not constrained to match argument count).
     * Maintains linearity constraints:
     * - All arguments must appear at least once across all return values
     * - No argument can appear more than once in any single return value
     */
    def applyMulti[AT <: Tuple, DT <: Tuple, CT <: Tuple, RT <: Tuple, RQT <: Tuple]
    (args: AT)
    (fns: ToLinearRef[AT] => RQT)
    (using @implicitNotFound("Cannot extract result types from RQT") ev1: RT =:= ExtractResultTypes[RQT])
    (using @implicitNotFound("Cannot extract dependencies from RQT") ev1b: DT =:= ExtractDependencyTypes[RQT])
    (using @implicitNotFound("Cannot extract consumption states from RQT") ev1c: CT =:= ExtractConsumedTypes[RQT])
    (using @implicitNotFound("Cannot extract dependencies, is the query affine?") ev2: InverseMapDeps[RQT] =:= DT)
    (using @implicitNotFound("Failed to match restricted types: ${RQT}") ev3: RQT =:= ToRestricted[RT, DT, CT])
    (using @implicitNotFound("Recursive definitions must be linear: ${RT}") ev4: ExpectedResult[AT] <:< ActualResult[RQT])
    (using @implicitNotFound("All return values must have consumption state of length 0 or 1") ev5: AllConsumedStatesAtMostOne[CT] =:= true): RT =
      val argsRefs = args.toArray.map(a => makeLinearRef(() => a))
      val refsTuple = Tuple.fromArray(argsRefs).asInstanceOf[ToLinearRef[AT]]
      val exec = fns(refsTuple)
      println(exec)
      tupleExecute(exec).asInstanceOf[RT]

    /**
     * applyConsumedMulti - variant of applyMulti that requires all return values to be consumed.
     * Allows arbitrary number of return values while ensuring all are consumed exactly once.
     */
    def applyConsumedMulti[AT <: Tuple, DT <: Tuple, CT <: Tuple, RT <: Tuple, RQT <: Tuple]
    (args: AT)
    (fns: ToLinearRef[AT] => RQT)
    (using @implicitNotFound("Cannot extract result types from RQT") ev1: RT =:= ExtractResultTypes[RQT])
    (using @implicitNotFound("Cannot extract dependencies from RQT") ev1b: DT =:= ExtractDependencyTypes[RQT])
    (using @implicitNotFound("Cannot extract consumption states from RQT") ev1c: CT =:= ExtractConsumedTypes[RQT])
    (using @implicitNotFound("Cannot extract dependencies, is the query affine?") ev2: InverseMapDeps[RQT] =:= DT)
    (using @implicitNotFound("Failed to match restricted types: ${RQT}") ev3: RQT =:= ToRestricted[RT, DT, CT])
    (using @implicitNotFound("Recursive definitions must be linear: ${RT}") ev4: ExpectedResult[AT] <:< ActualResult[RQT])
    (using @implicitNotFound("All return values must be consumed (consumption state length must be exactly 1)") ev5: AllConsumedStatesExactlyOne[CT] =:= true): RT =
      val argsRefs = args.toArray.map(a => makeLinearRef(() => a))
      val refsTuple = Tuple.fromArray(argsRefs).asInstanceOf[ToLinearRef[AT]]
      val exec = fns(refsTuple)
      println(exec)
      tupleExecute(exec).asInstanceOf[RT]
