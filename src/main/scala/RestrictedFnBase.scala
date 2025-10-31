package linearfn

import Utils.*
import scala.annotation.implicitNotFound

// ============================================================================
// Constraint Type Definitions for customApply
// ============================================================================

/**
 * Vertical linearity constraints - track consumption state through method chains
 */
sealed trait VerticalConstraint
object VerticalConstraint:
  /** C must have length = 1 (consumed exactly once) */
  case object Linear extends VerticalConstraint
  /** C must have length ≤ 1 (consumed at most once) */
  case object Affine extends VerticalConstraint
  /** C must have length ≥ 1 (consumed at least once) */
  case object Relevant extends VerticalConstraint

/**
 * Horizontal linearity constraints - track argument distribution across returns
 * Each combines two orthogonal checks (ForAll/ForEach + Affine/Relevant)
 */
sealed trait HorizontalConstraint
object HorizontalConstraint:
  /**
   * For-all-relevant + For-each-affine (current default)
   * - Each arg appears ≥1 times across all returns
   * - Each arg appears ≤1 time per return
   * Example: (a, b) → (a, b, a) ✓
   */
  case object ForAllRelevantForEachAffine extends HorizontalConstraint

  /**
   * For-all-relevant + For-all-affine (traditional linear types)
   * - Each arg appears ≥1 times across all returns
   * - Each arg appears ≤1 time total
   * Example: (a, b) → (a, b) ✓, (a, b) → (a, b, a) ✗
   */
  case object ForAllRelevantForAllAffine extends HorizontalConstraint

  /**
   * For-each-relevant + For-each-affine (uniform recursion)
   * - Each arg appears ≥1 times in every return
   * - Each arg appears ≤1 time per return
   * Example: (a, b) → (a+b, a+b) ✓, (a, b) → (a, b) ✗
   */
  case object ForEachRelevantForEachAffine extends HorizontalConstraint

  /**
   * For-each-relevant + For-all-affine (impractical - only 1 return)
   * - Each arg appears ≥1 times in every return
   * - Each arg appears ≤1 time total
   * Example: Only works with single return value
   */
  case object ForEachRelevantForAllAffine extends HorizontalConstraint

/**
 * Abstract base for all RestrictedFn implementations.
 *
 * This class implements the core logic for enforcing substructural constraints:
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

abstract class RestrictedFnBase:

  // Abstract type with upper bound constraint
  // All implementations must extend RestrictedBase
  type Restricted[A, D <: Tuple, C <: Tuple] <: RestrictedBase[A, D, C]

  // Factory method for creating RestrictedRef instances
  protected def makeRestrictedRef[A, D <: Tuple, C <: Tuple](fn: () => A): Restricted[A, D, C]

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

  // Convert argument tuple to RestrictedRefs with single-element dependency tuples
  // Each argument gets a unique index: arg0 → (0), arg1 → (1), etc.
  type ToRestrictedRef[AT <: Tuple] = Tuple.Map[ZipWithIndex[AT], [T] =>> T match
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

  // RELEVANT CONSTRAINT: Check that all consumption states are consumed at least once (length ≥ 1)
  // Used by customApply with VerticalConstraint.Relevant
  // C must have length ≥ 1
  type AllConsumedStatesAtLeastOne[CT <: Tuple] <: Boolean = CT match
    case EmptyTuple => true
    case h *: t => Tuple.Size[h] match
      case 0 => false
      case _ => AllConsumedStatesAtLeastOne[t]

  // ============================================================================
  // Constraint Checking for customApply
  // ============================================================================

  // Check vertical constraint satisfaction based on VerticalConstraint type
  type CheckVerticalConstraint[VC <: VerticalConstraint, CT <: Tuple] <: Boolean = VC match
    case VerticalConstraint.Linear.type => AllConsumedStatesExactlyOne[CT]
    case VerticalConstraint.Affine.type => AllConsumedStatesAtMostOne[CT]
    case VerticalConstraint.Relevant.type => AllConsumedStatesAtLeastOne[CT]

  // Check strict constraint (n args → n returns with same types)
  type CheckStrictConstraint[Strict <: Boolean, AT <: Tuple, RT <: Tuple] = Strict match
    case true => Tuple.Size[AT] =:= Tuple.Size[RT]
    case false => true =:= true  // Always satisfied when strict=false

  // Helper: Check for-all-relevant (all args used at least once across all returns)
  // Returns true if constraint is satisfied (all args used)
  type CheckForAllRelevant[AT <: Tuple, RQT <: Tuple] =
    ExpectedResult[AT] <:< ActualResult[RQT]

  // Helper: Check for-all-affine (no arg used more than once total)
  // Count total uses of each argument across all returns
  type CheckForAllAffine[AT <: Tuple, RQT <: Tuple] =
    AllArgsUsedAtMostOnceTotal[GenerateIndices[0, Tuple.Size[AT]], RQT]

  type AllArgsUsedAtMostOnceTotal[Indices <: Tuple, RQT <: Tuple] <: Boolean = Indices match
    case EmptyTuple => true
    case idx *: rest =>
      CountArgUsesInAllReturns[idx, RQT] match
        case 0 => AllArgsUsedAtMostOnceTotal[rest, RQT]
        case 1 => AllArgsUsedAtMostOnceTotal[rest, RQT]
        case _ => false

  // Collect all occurrences of Idx across all returns as a tuple, then count with Tuple.Size
  type CountArgUsesInAllReturns[Idx, RQT <: Tuple] =
    Tuple.Size[CollectArgUsesInAllReturns[Idx, RQT]]

  type CollectArgUsesInAllReturns[Idx, RQT <: Tuple] <: Tuple = RQT match
    case EmptyTuple => EmptyTuple
    case h *: t =>
      Tuple.Concat[
        CollectArgUsesInReturn[Idx, h],
        CollectArgUsesInAllReturns[Idx, t]
      ]

  type CollectArgUsesInReturn[Idx, R] <: Tuple = LiftInnerType[R] match
    case (_, d, _) => CollectOccurrences[Idx, d]

  // Collect all occurrences of Elem in tuple T as a tuple of Units
  type CollectOccurrences[Elem, T <: Tuple] <: Tuple = T match
    case EmptyTuple => EmptyTuple
    case Elem *: tail => Unit *: CollectOccurrences[Elem, tail]
    case _ *: tail => CollectOccurrences[Elem, tail]

  // Count occurrences by collecting them as a tuple, then taking size
  type CountOccurrences[Elem, T <: Tuple] =
    Tuple.Size[CollectOccurrences[Elem, T]]

  // Helper: Check for-each-affine (no duplicates in any single return)
  type CheckForEachAffine[DT <: Tuple, RQT <: Tuple] =
    InverseMapDeps[RQT] =:= DT  // This already checks for duplicates

  // Helper: Check for-each-relevant (all args in every return)
  type CheckForEachRelevant[AT <: Tuple, RT <: Tuple] =
    AllReturnsContainAllArgs[GenerateIndices[0, Tuple.Size[AT]], RT]

  type AllReturnsContainAllArgs[Indices <: Tuple, RT <: Tuple] <: Boolean = RT match
    case EmptyTuple => true
    case h *: t =>
      ReturnContainsAllIndices[Indices, h] match
        case true => AllReturnsContainAllArgs[Indices, t]
        case false => false

  type ReturnContainsAllIndices[Indices <: Tuple, R] <: Boolean = LiftInnerType[R] match
    case (_, d, _) => AllIndicesInDeps[Indices, d]

  type AllIndicesInDeps[Indices <: Tuple, D <: Tuple] <: Boolean = Indices match
    case EmptyTuple => true
    case idx *: rest =>
      Contains[idx, D] match
        case true => AllIndicesInDeps[rest, D]
        case false => false

  type Contains[Elem, T <: Tuple] <: Boolean = T match
    case EmptyTuple => false
    case Elem *: _ => true
    case _ *: tail => Contains[Elem, tail]

  // Check first component of horizontal constraint (Relevant check)
  type CheckHorizontalRelevant[
    HC <: HorizontalConstraint,
    AT <: Tuple,
    RT <: Tuple,
    RQT <: Tuple
  ] = HC match
    case HorizontalConstraint.ForAllRelevantForEachAffine.type =>
      CheckForAllRelevant[AT, RQT]
    case HorizontalConstraint.ForAllRelevantForAllAffine.type =>
      CheckForAllRelevant[AT, RQT]
    case HorizontalConstraint.ForEachRelevantForEachAffine.type =>
      CheckForEachRelevant[AT, RT]
    case HorizontalConstraint.ForEachRelevantForAllAffine.type =>
      CheckForEachRelevant[AT, RT]

  // Check second component of horizontal constraint (Affine check)
  type CheckHorizontalAffine[
    HC <: HorizontalConstraint,
    AT <: Tuple,
    DT <: Tuple,
    RQT <: Tuple
  ] = HC match
    case HorizontalConstraint.ForAllRelevantForEachAffine.type =>
      CheckForEachAffine[DT, RQT]
    case HorizontalConstraint.ForAllRelevantForAllAffine.type =>
      CheckForAllAffine[AT, RQT]
    case HorizontalConstraint.ForEachRelevantForEachAffine.type =>
      CheckForEachAffine[DT, RQT]
    case HorizontalConstraint.ForEachRelevantForAllAffine.type =>
      CheckForAllAffine[AT, RQT]

  // ============================================================================
  // RestrictedFn Methods: Combining Vertical and Horizontal Constraints
  // ============================================================================

  object RestrictedFn:
    /**
     * applyImpl - Core implementation without constraints.
     * Private method used by all public apply variants.
     */
    private def applyImpl[AT <: Tuple, RT <: Tuple, RQT <: Tuple](
      args: AT
    )(fns: ToRestrictedRef[AT] => RQT): RT =
      val argsRefs = args.toArray.map(a => makeRestrictedRef(() => a))
      val refsTuple = Tuple.fromArray(argsRefs).asInstanceOf[ToRestrictedRef[AT]]
      val exec = fns(refsTuple)
      tupleExecute(exec).asInstanceOf[RT]

    /**
     * customApply - Fully customizable substructural constraint function with user-specified constraints.
     *
     * Allows users to specify:
     * - Vertical constraint (Linear/Affine/Relevant)
     * - Horizontal constraint (one of 4 pre-defined combinations)
     *
     * This method does NOT enforce strict size checking (n args = n returns).
     * Use `strictApply` if you need size and argument/return type constraints.
     *
     * Usage:
     * {{{
     * RestrictedFn.customApply(
     *   (vertical = VerticalConstraint.Affine,
     *    horizontal = HorizontalConstraint.ForAllRelevantForEachAffine)
     * )((a, b))(refs => (refs._1, refs._2, refs._1))  // 3 returns from 2 args OK
     * }}}
     */
    def customApply[
      AT <: Tuple, DT <: Tuple, CT <: Tuple, RT <: Tuple, RQT <: Tuple,
      VC <: VerticalConstraint,
      HC <: HorizontalConstraint
    ](
      options: (vertical: VC, horizontal: HC)
    )(args: AT)(fns: ToRestrictedRef[AT] => RQT)(
      using
        // Basic type extraction evidences
        @implicitNotFound(ErrorMsg.invalidResultTypes)
        ev1: RT =:= ExtractResultTypes[RQT],

        @implicitNotFound(ErrorMsg.invalidDependencyTypes)
        ev1b: DT =:= ExtractDependencyTypes[RQT],

        @implicitNotFound(ErrorMsg.invalidConsumptionTypes)
        ev1c: CT =:= ExtractConsumedTypes[RQT],

        @implicitNotFound(ErrorMsg.invalidRestrictedTypes)
        ev3: RQT =:= ToRestricted[RT, DT, CT],

        // Vertical constraint evidence
        @implicitNotFound(ErrorMsg.verticalConstraintFailed)
        evV: CheckVerticalConstraint[VC, CT] =:= true,

        // Horizontal constraint evidences (two separate checks)
        @implicitNotFound(ErrorMsg.horizontalRelevanceFailed)
        evHR: CheckHorizontalRelevant[HC, AT, RT, RQT],

        @implicitNotFound(ErrorMsg.horizontalAffineFailed)
        evHA: CheckHorizontalAffine[HC, AT, DT, RQT]
    ): RT = applyImpl[AT, RT, RQT](args)(fns)

    /**
     * apply - Convenience method for the default linear function behavior.
     *
     * VERTICAL (Consumption): AFFINE - allows unconsumed values (at most once consumed)
     * HORIZONTAL (Dependencies): FOR-ALL-RELEVANT + FOR-EACH-AFFINE
     * STRICT: false - allows any number of return values
     *
     * This is the standard apply method. It allows flexible return counts while enforcing
     * that all arguments are used and each argument appears at most once per return.
     *
     * Usage:
     * {{{
     * RestrictedFn.apply((a, b))(refs => (refs._1, refs._2, refs._1))  // 3 returns from 2 args OK
     * }}}
     */
    def apply[AT <: Tuple, DT <: Tuple, CT <: Tuple, RT <: Tuple, RQT <: Tuple]
    (args: AT)(fns: ToRestrictedRef[AT] => RQT)(
      using
        ev1: RT =:= ExtractResultTypes[RQT],
        ev1b: DT =:= ExtractDependencyTypes[RQT],
        ev1c: CT =:= ExtractConsumedTypes[RQT],
        ev3: RQT =:= ToRestricted[RT, DT, CT],

        @implicitNotFound(ErrorMsg.verticalConstraintFailed)
        evV: CheckVerticalConstraint[VerticalConstraint.Affine.type, CT] =:= true,

        @implicitNotFound(ErrorMsg.horizontalRelevanceFailed)
        evHR: CheckHorizontalRelevant[HorizontalConstraint.ForAllRelevantForEachAffine.type, AT, RT, RQT],

        @implicitNotFound(ErrorMsg.horizontalAffineFailed)
        evHA: CheckHorizontalAffine[HorizontalConstraint.ForAllRelevantForEachAffine.type, AT, DT, RQT]
    ): RT = applyImpl[AT, RT, RQT](args)(fns)

    /**
     * strictApply - Convenience method for strict linear functions (n args → n returns).
     *
     * VERTICAL (Consumption): AFFINE - allows unconsumed values (at most once)
     * HORIZONTAL (Dependencies): FOR-ALL-RELEVANT + FOR-EACH-AFFINE
     * STRICT: true - requires n arguments → n returns with same number
     *
     * This matches the original `apply` behavior and is useful when you want to enforce
     * that the number of returns matches the number of arguments.
     *
     * Usage:
     * {{{
     * RestrictedFn.strictApply((a, b))(refs => (refs._1, refs._2))  // Must be 2 args → 2 returns
     * }}}
     */
    def strictApply[AT <: Tuple, DT <: Tuple, CT <: Tuple, RT <: Tuple, RQT <: Tuple]
    (args: AT)(fns: ToRestrictedRef[AT] => RQT)(
      using
        ev1: RT =:= ExtractResultTypes[RQT],
        ev1b: DT =:= ExtractDependencyTypes[RQT],
        ev1c: CT =:= ExtractConsumedTypes[RQT],
        ev3: RQT =:= ToRestricted[RT, DT, CT],

        @implicitNotFound(ErrorMsg.verticalConstraintFailed)
        evV: CheckVerticalConstraint[VerticalConstraint.Affine.type, CT] =:= true,

        @implicitNotFound(ErrorMsg.horizontalRelevanceFailed)
        evHR: CheckHorizontalRelevant[HorizontalConstraint.ForAllRelevantForEachAffine.type, AT, RT, RQT],

        @implicitNotFound(ErrorMsg.horizontalAffineFailed)
        evHA: CheckHorizontalAffine[HorizontalConstraint.ForAllRelevantForEachAffine.type, AT, DT, RQT],

        @implicitNotFound(ErrorMsg.strictFnFailed)
        evStrict: CheckStrictConstraint[true, AT, RT]
    ): RT = applyImpl[AT, RT, RQT](args)(fns)
