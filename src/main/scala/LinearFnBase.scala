package linearfn

import Utils.*
import scala.annotation.implicitNotFound

/**
 * Abstract base for all LinearFn implementations.
 *
 * This class implements the core logic for enforcing linearity constraints:
 *
 * 1. VERTICAL LINEARITY (Consumption Tracking):
 *    - Tracks the lifecycle of a single value through a CHAIN of method calls
 *    - C = EmptyTuple: unconsumed (operations available)
 *    - C = Tuple1[true]: consumed (only special operations allowed)
 *    - Modes:
 *      * Affine (apply/applyMulti): allows unconsumed values (C can be any length > 0)
 *      * Linear (applyConsumed/applyConsumedMulti): requires consumed values (C must be length 1)
 *      * Relevant (not implemented here): requires consumed values at least once (C can be of any length > 1)
 *
 * 2. HORIZONTAL LINEARITY (Cross-argument Dependency Tracking - D Parameter):
 *    - Tracks how multiple arguments are distributed across return values
 *    - D contains indices of input arguments this value depends on (e.g., (0, 1))
 *    - Constraint: FOR-ALL-RELEVANT + FOR-EACH-AFFINE:
 *      * For-all-relevant: Each argument must appear ≥1 times across all returns (ExpectedResult <:< ActualResult)
 *      * For-each-affine: Each argument can appear ≤1 time per return (InverseMapDeps checks for duplicates)
 *
 * The combination enables:
 * - (a, b) → (a, b) ✓ (each used once)
 * - (a, b) → (a, b, a) ✓ (a used in multiple returns, but ≤1 per return)
 * - (a, b) → (a+b, x) ✓ (both used in first return)
 * - (a, b) → (a, a) ✗ (b unused - violates for-all-relevant)
 * - (a, b) → (a+a, b) ✗ (a used twice in first return - violates for-each-affine)
 *
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
  // ============================================================================
  // HORIZONTAL LINEARITY: Dependency Tracking (D Parameter)
  // ============================================================================

  // Convert argument tuple to LinearRefs with single-element dependency tuples
  // Each argument gets a unique index: arg0 → (0), arg1 → (1), etc.
  type ToLinearRef[AT <: Tuple] = Tuple.Map[ZipWithIndex[AT], [T] =>> T match
    case (elem, index) => Restricted[elem, Tuple1[index], EmptyTuple]
  ]

  // FOR-EACH-AFFINE CONSTRAINT: Check that each return value is affine
  // Maps each return to a Boolean indicating if it has duplicate dependencies
  // If any return has duplicates (true), compilation fails
  // Example: (a, a) has duplicates → false; (a, b) has no duplicates → true
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

  // FOR-ALL-RELEVANT CONSTRAINT: Check that all arguments are used somewhere
  // ExpectedResult: Union of all argument indices {0, 1, 2, ...}
  // ActualResult: Union of all dependencies actually used across all returns
  // Constraint: ExpectedResult <:< ActualResult ensures every arg appears at least once
  // Example: args=(a,b), returns=(a,a) → ActualResult={0}, ExpectedResult={0,1} → FAIL (b unused)
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

  // ============================================================================
  // VERTICAL LINEARITY: Consumption Tracking (C Parameter)
  // ============================================================================

  // AFFINE CONSTRAINT: Check that all consumption states are valid (length 0 or 1)
  // Used by apply/applyMulti - allows unconsumed values
  // C = EmptyTuple (length 0): unconsumed
  // C = Tuple1[true] (length 1): consumed
  type AllConsumedStatesAtMostOne[CT <: Tuple] <: Boolean = CT match
    case EmptyTuple => true
    case h *: t => Tuple.Size[h] match
      case 0 => AllConsumedStatesAtMostOne[t]
      case 1 => AllConsumedStatesAtMostOne[t]
      case _ => false

  // LINEAR CONSTRAINT: Check that all consumption states are consumed (length 1)
  // Used by applyConsumed/applyConsumedMulti - requires all values consumed
  // Only C = Tuple1[true] (length 1) is allowed
  type AllConsumedStatesExactlyOne[CT <: Tuple] <: Boolean = CT match
    case EmptyTuple => true
    case h *: t => Tuple.Size[h] match
      case 1 => AllConsumedStatesExactlyOne[t]
      case _ => false

  // ============================================================================
  // LinearFn Methods: Combining Vertical and Horizontal Constraints
  // ============================================================================
  object LinearFn:
    /**
     * apply - Standard linear function with equal argument and return counts.
     *
     * VERTICAL (Consumption): AFFINE - allows unconsumed values (AllConsumedStatesAtMostOne)
     * HORIZONTAL (Dependencies): FOR-ALL-RELEVANT + FOR-EACH-AFFINE
     *   - ev2 (InverseMapDeps): Each argument ≤1 per return (for-each-affine)
     *   - ev4 (ExpectedResult <:< ActualResult): Each argument ≥1 across all returns (for-all-relevant)
     *
     * Constraint: n arguments → n returns (Tuple.Size[AT] =:= Tuple.Size[RT])
     */
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
     *
     * VERTICAL (Consumption): LINEAR - requires consumed values (AllConsumedStatesExactlyOne)
     * HORIZONTAL (Dependencies): FOR-ALL-RELEVANT + FOR-EACH-AFFINE (same as apply)
     *
     * Constraint: n arguments → n returns (Tuple.Size[AT] =:= Tuple.Size[RT])
     *
     * Use this when you want to ensure that all results have been consumed exactly once
     * (e.g., files must be closed, transactions must be committed).
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
     *
     * VERTICAL (Consumption): AFFINE - allows unconsumed values (AllConsumedStatesAtMostOne)
     * HORIZONTAL (Dependencies): FOR-ALL-RELEVANT + FOR-EACH-AFFINE (same as apply)
     *
     * NO constraint on return count: n arguments → any number of returns
     *
     * Enables flexible patterns:
     * - (a, b) → (a, b, a) ✓ (3 returns from 2 args)
     * - (a, b, c) → (a) ✗ (b, c unused - violates for-all-relevant)
     * - (a, b) → (a+a, b) ✗ (a used twice in first return - violates for-each-affine)
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
     *
     * VERTICAL (Consumption): LINEAR - requires consumed values (AllConsumedStatesExactlyOne)
     * HORIZONTAL (Dependencies): FOR-ALL-RELEVANT + FOR-EACH-AFFINE (same as applyMulti)
     *
     * NO constraint on return count: n arguments → any number of returns
     *
     * Use this when you want flexible return counts but need to ensure all results are consumed.
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
