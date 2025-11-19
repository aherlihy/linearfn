package linearfn

import Utils.*
import scala.annotation.implicitNotFound

// ============================================================================
// Multiplicity Type Definitions
// ============================================================================

/**
 * Multiplicity constraints - track how many times a value can be consumed
 */
sealed trait Multiplicity
object Multiplicity:
  /** Must be consumed exactly once */
  sealed trait Linear extends Multiplicity
  /** Can be consumed at most once */
  sealed trait Affine extends Multiplicity
  /** Must be consumed at least once */
  sealed trait Relevant extends Multiplicity

  // Witness objects for explicit multiplicity selection
  object Linear extends Linear
  object Affine extends Affine
  object Relevant extends Relevant

// ============================================================================
// Custom Connective Type Definitions
// ============================================================================

/**
 * Base trait for all custom connectives.
 * A connective composes multiple Restricted values with specific multiplicity constraints.
 *
 * @tparam ForEachM - Multiplicity constraint applied to each individual return value
 * @tparam ForAllM - Multiplicity constraint applied across all return values
 */
trait CustomConnective[ForEachM <: Multiplicity, ForAllM <: Multiplicity]:
  type Composed

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

  /**
   * ComposedConnective: The result of composing Restricted values with a CustomConnective.
   * This is what gets returned from connective.compose().
   * Always has Consumed = Tuple1[true] (must be consumed)
   */
  case class ComposedConnective[
    RT <: Tuple,
    DT <: Tuple,
    ForEachM <: Multiplicity,
    ForAllM <: Multiplicity
  ](
    values: RT
  ) extends RestrictedBase[Tuple, DT, Tuple1[true]]:
    def execute(): Tuple =
      tupleExecute(values)

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
    case ComposedConnective[rt, dt, forEachM, forAllM] => (Tuple, dt, Tuple1[true])
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

  // Extract consumed types but mark unrestricted values (d = EmptyTuple) as Tuple1[true]
  // This keeps tuple lengths consistent and treats unrestricted values as always satisfying multiplicity
  type ExtractConsumedTypesForChecking[RQT <: Tuple, DT <: Tuple] <: Tuple = (RQT, DT) match
    case (EmptyTuple, EmptyTuple) => EmptyTuple
    case (h *: tail, d *: dtail) => d match
      case EmptyTuple => Tuple1[true] *: ExtractConsumedTypesForChecking[tail, dtail]  // Unrestricted - mark as consumed
      case _ => LiftInnerType[h] match
        case (_, _, c) => c *: ExtractConsumedTypesForChecking[tail, dtail]

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
  // NOTE: This is used with ExtractConsumedTypesWithDeps which filters out values with no dependencies,
  // so we might see fewer elements than total return values
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
  // Used by customApply with Multiplicity.Relevant
  // C must have length ≥ 1
  type AllConsumedStatesAtLeastOne[CT <: Tuple] <: Boolean = CT match
    case EmptyTuple => true
    case h *: t => Tuple.Size[h] match
      case 0 => false
      case _ => AllConsumedStatesAtLeastOne[t]

  // ============================================================================
  // Constraint Checking for customApply
  // ============================================================================

  // Check multiplicity constraint satisfaction based on Multiplicity type
  type CheckMultiplicity[M <: Multiplicity, CT <: Tuple] <: Boolean = M match
    case Multiplicity.Linear => AllConsumedStatesExactlyOne[CT]
    case Multiplicity.Affine => AllConsumedStatesAtMostOne[CT]
    case Multiplicity.Relevant => AllConsumedStatesAtLeastOne[CT]

  // Check ForAll-Affine constraint based on Multiplicity type
  // For Relevant, skip the ForAll-Affine check (allow arguments to be used multiple times)
  type CheckForAllAffineForMultiplicity[M <: Multiplicity, AT <: Tuple, RQT <: Tuple] <: Boolean = M match
    case Multiplicity.Relevant => true  // Skip ForAll-Affine for Relevant
    case _ => AllArgsUsedAtMostOnceTotal[GenerateIndices[0, Tuple.Size[AT]], RQT]

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

  // Check ForAll multiplicity constraint (across all returns)
  type CheckForAllMultiplicity[
    ForAllM <: Multiplicity,
    AT <: Tuple,
    RT <: Tuple,
    RQT <: Tuple
  ] = ForAllM match
    case Multiplicity.Linear => CheckForAllRelevant[AT, RQT]  // Must appear exactly once total (=relevant + affine)
    case Multiplicity.Affine => CheckForAllAffine[AT, RQT]    // Can appear at most once total
    case Multiplicity.Relevant => CheckForAllRelevant[AT, RQT] // Must appear at least once total

  // Check ForEach multiplicity constraint (per individual return)
  type CheckForEachMultiplicity[
    ForEachM <: Multiplicity,
    AT <: Tuple,
    DT <: Tuple,
    RT <: Tuple,
    RQT <: Tuple
  ] = ForEachM match
    case Multiplicity.Linear => CheckForEachLinear[AT, DT, RT, RQT]  // Each return: exactly once (relevant + affine)
    case Multiplicity.Affine => CheckForEachAffine[DT, RQT]          // Each return: at most once
    case Multiplicity.Relevant => CheckForEachRelevant[AT, RT]       // Each return: at least once

  // Helper: Check for-each-linear (each arg in every return, no duplicates per return)
  // This is the conjunction of for-each-relevant AND for-each-affine
  type CheckForEachLinear[AT <: Tuple, DT <: Tuple, RT <: Tuple, RQT <: Tuple] =
    (CheckForEachRelevant[AT, RT], CheckForEachAffine[DT, RQT])

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
     * LinearFn: A type alias for linear functions.
     *
     * This is the library's contribution - a clean name for linear functions
     * that take restricted references and return a ComposedConnective.
     */
    type LinearFn[AT <: Tuple, RQT] = ToRestrictedRef[AT] => RQT

    /**
     * LinearFnBuilder: Encodes base linearity constraints with custom connectives.
     *
     * User code requests a builder via `using`, and the given instance
     * will be found IFF all base linearity constraints are satisfied.
     *
     * RQT must be a ComposedConnective that defines the composition strategy.
     */
    @implicitNotFound(ErrorMsg.substructuralContstraintFailed)
    trait LinearFnBuilder[M <: Multiplicity, AT <: Tuple, RQT]:
      def execute(args: AT)(fns: LinearFn[AT, RQT]): RQT

    object LinearFnBuilder:
      // Builder for ComposedConnective - enforces custom connective constraints
      given connectiveBuilder[
        M <: Multiplicity,
        AT <: Tuple,
        RT <: Tuple,
        DT <: Tuple,
        ForEachM <: Multiplicity,
        ForAllM <: Multiplicity
      ](using
        // Input multiplicity constraint (on arguments)
        @implicitNotFound(ErrorMsg.multiplicityConstraintFailed)
        evM: CheckMultiplicity[M, Tuple.Map[RT, [_] =>> Tuple1[true]]] =:= true,

        // Connective multiplicity constraints
        @implicitNotFound(ErrorMsg.compositionForAllFailed)
        evForAll: CheckForAllMultiplicity[ForAllM, AT, RT, RT],

        @implicitNotFound(ErrorMsg.compositionForEachFailed)
        evForEach: CheckForEachMultiplicity[ForEachM, AT, DT, RT, RT]
      ): LinearFnBuilder[M, AT, ComposedConnective[RT, DT, ForEachM, ForAllM]] with
        def execute(args: AT)(fns: LinearFn[AT, ComposedConnective[RT, DT, ForEachM, ForAllM]]): ComposedConnective[RT, DT, ForEachM, ForAllM] =
          val argsRefs = (0 until args.size).map(i => makeRestrictedRef(() => args.productElement(i).asInstanceOf[Any])).toArray
          val refsTuple = Tuple.fromArray(argsRefs).asInstanceOf[ToRestrictedRef[AT]]
          fns(refsTuple)

      // Builder for plain tuples - treats them as a built-in connective with ForAll-Relevant + ForAll-Affine
      // This is the implicit connective for regular tuple returns:
      // - Each argument must be used at least once across all returns (ForAll-Relevant)
      // - Each argument can be used at most once across all returns (ForAll-Affine)
      given tupleBuilder[
        M <: Multiplicity,
        AT <: Tuple,
        RQT <: Tuple
      ](using
        // Input multiplicity constraint (on arguments)
        @implicitNotFound(ErrorMsg.multiplicityConstraintFailed)
        evM: CheckMultiplicity[M, ExtractConsumedTypes[RQT]] =:= true,

        // ForAll-Relevant: all arguments must be used at least once across all returns
        @implicitNotFound(ErrorMsg.compositionForAllFailed)
        evForAllRelevant: CheckForAllMultiplicity[Multiplicity.Relevant, AT, RQT, RQT],

        // ForAll-Affine: no argument used more than once across all returns
        @implicitNotFound(ErrorMsg.compositionForEachFailed)
        evForAllAffine: CheckForAllMultiplicity[Multiplicity.Affine, AT, RQT, RQT]
      ): LinearFnBuilder[M, AT, RQT] with
        def execute(args: AT)(fns: LinearFn[AT, RQT]): RQT =
          val argsRefs = args.toArray.map(a => makeRestrictedRef(() => a))
          val refsTuple = Tuple.fromArray(argsRefs).asInstanceOf[ToRestrictedRef[AT]]
          fns(refsTuple)

    /**
     * apply: General-purpose linear function application with configurable multiplicity.
     *
     * This is a convenience method for applying linear functions with the standard pattern:
     * 1. Wrap arguments in Restricted types
     * 2. Call the function
     * 3. Execute and unwrap the result
     *
     * Enforces the built-in connective semantics for plain tuples:
     * - Input multiplicity M (Linear/Affine/Relevant) on consumed state
     * - ForAll-Relevant: all arguments used at least once across all returns
     * - ForAll-Affine: no argument used more than once across all returns
     *
     * These semantics treat plain tuples as a built-in connective where each argument
     * is affine and relevant across the ENTIRE tuple (not per-return).
     * For custom implementations with different semantics, use LinearFnBuilder directly.
     *
     * @tparam M The multiplicity constraint (Linear, Affine, or Relevant)
     * @tparam AT The argument tuple type
     * @tparam RQT The return type (tuple of Restricted values)
     * @param args The argument tuple
     * @param fns The function from restricted references to return values
     * @return The tuple of executed results
     */
    def apply[M <: Multiplicity, AT <: Tuple, RT <: Tuple, DT <: Tuple, CT <: Tuple, RQT <: Tuple, CheckCT <: Tuple](
      multiplicity: M
    )(args: AT)(fns: LinearFn[AT, RQT])(
      using
        ev1: RT =:= ExtractResultTypes[RQT],
        ev1b: DT =:= ExtractDependencyTypes[RQT],
        ev1c: CT =:= ExtractConsumedTypes[RQT],
        ev3: RQT =:= ToRestricted[RT, DT, CT],
        evCheckCT: CheckCT =:= ExtractConsumedTypesForChecking[RQT, DT],
        @implicitNotFound(ErrorMsg.multiplicityConstraintFailed)
        ev2c: CheckMultiplicity[M, CheckCT] =:= true,
        @implicitNotFound(ErrorMsg.compositionForAllFailed)
        evForAllRelevant: ExpectedResult[AT] <:< ActualResult[RQT],
        @implicitNotFound(ErrorMsg.compositionForEachFailed)
        evForAllAffine: CheckForAllAffineForMultiplicity[M, AT, RQT] =:= true
    ): ExtractResultTypes[RQT] = {
      val argsRefs = args.toArray.map(a => makeRestrictedRef(() => a))
      val refsTuple = Tuple.fromArray(argsRefs).asInstanceOf[ToRestrictedRef[AT]]
      val exec = fns(refsTuple)
      tupleExecute(exec).asInstanceOf[ExtractResultTypes[RQT]]
    }

