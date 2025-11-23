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
  /** Can be consumed any number of times (no restrictions) */
  sealed trait Unrestricted extends Multiplicity

  // Witness objects for explicit multiplicity selection
  object Linear extends Linear
  object Affine extends Affine
  object Relevant extends Relevant
  object Unrestricted extends Unrestricted

// ============================================================================
// Custom Connective Type Definitions
// ============================================================================

/**
 * Abstract base for all RestrictedFn implementations.
 *
 * This class implements the core logic for enforcing substructural constraints:
 *
 * HORIZONTAL LINEARITY (Cross-argument Dependency Tracking - D Parameter):
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
trait RestrictedBase[A, D <: Tuple]:
  def execute(): A

abstract class RestrictedFnBase:

  // Abstract type with upper bound constraint
  // All implementations must extend RestrictedBase
  type Restricted[A, D <: Tuple] <: RestrictedBase[A, D]

  /**
   * ComposedConnective: The result of composing Restricted values with a CustomConnective.
   * This is what gets returned from connective.compose().
   */
  case class ComposedConnective[
    RT <: Tuple,
    DT <: Tuple,
    ForEachM <: Multiplicity,
    ForAllM <: Multiplicity
  ](
    values: RT
  ) extends RestrictedBase[Tuple, DT]:
    def execute(): Tuple =
      tupleExecute(values)

  // Factory method for creating RestrictedRef instances
  protected def makeRestrictedRef[A, D <: Tuple](fn: () => A): Restricted[A, D]

  // Execute a Restricted value
  protected def executeRestricted[A, D <: Tuple](r: Restricted[A, D]): A =
    r.execute()

  // Helper match type to extract inner type and dependencies from potentially nested containers
  // Returns (innermost_type, dependencies)
  // This is the PRIMARY place to add new container types for lifting support
  type LiftInnerType[T] = T match
    case Restricted[a, d] => (a, d)
    case ComposedConnective[rt, dt, forEachM, forAllM] => (Tuple, dt)
    case List[inner] => LiftInnerType[inner] match
      case (a, d) => (List[a], d)
    case Option[inner] => LiftInnerType[inner] match
      case (a, d) => (Option[a], d)
    case Vector[inner] => LiftInnerType[inner] match
      case (a, d) => (Vector[a], d)

  // Common match types
  // ============================================================================
  // HORIZONTAL LINEARITY: Dependency Tracking (D Parameter)
  // ============================================================================

  // Convert argument tuple to RestrictedRefs with single-element dependency tuples
  // Each argument gets a unique index: arg0 → (0), arg1 → (1), etc.
  type ToRestrictedRef[AT <: Tuple] = Tuple.Map[ZipWithIndex[AT], [T] =>> T match
    case (elem, index) => Restricted[elem, Tuple1[index]]
  ]

  // FOR-EACH-AFFINE CONSTRAINT: Check that each return value is affine
  // Maps each return to a Boolean indicating if it has duplicate dependencies
  // If any return has duplicates (true), compilation fails
  // Example: (a, a) has duplicates → false; (a, b) has no duplicates → true
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
  // We can check against RestrictedBase since all Restricted types must extend it
  private def executeNested(value: Any): Any = value match
    // Check for containers BEFORE Restricted to handle nested cases
    case list: List[_] => list.map(executeNested)
    case opt: Option[_] => opt.map(executeNested)
    case vec: Vector[_] => vec.map(executeNested)
    case r: RestrictedBase[_, _] => r.execute()
    case other => other

  // Common tupleExecute implementation
  def tupleExecute[T <: Tuple](t: T): Tuple =
    t match
      case EmptyTuple => EmptyTuple
      case h *: tail => executeNested(h) *: tupleExecute(tail)

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
    case (_, d) => CollectOccurrences[Idx, d]

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
    case (_, d) => AllIndicesInDeps[Indices, d]

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
    case Multiplicity.Unrestricted => true                     // No constraint (always satisfied)

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
    case Multiplicity.Unrestricted => true                            // No constraint (always satisfied)

  // Helper: Check for-each-linear (each arg in every return, no duplicates per return)
  // This is the conjunction of for-each-relevant AND for-each-affine
  type CheckForEachLinear[AT <: Tuple, DT <: Tuple, RT <: Tuple, RQT <: Tuple] =
    (CheckForEachRelevant[AT, RT], CheckForEachAffine[DT, RQT])

  // ============================================================================
  // RestrictedFn Methods: Combining Vertical and Horizontal Constraints
  // ============================================================================

  object RestrictedFn:
    /**
     * LinearFn: A type alias for linear functions.
     *
     * This is the library's contribution - a clean name for linear functions
     * that take restricted references and return a ComposedConnective.
     */
    type LinearFn[AT <: Tuple, RQT] = ToRestrictedRef[AT] => RQT

    trait LinearFnBuilder[M <: Multiplicity, AT <: Tuple, CustomConnective]:
      def execute(args: AT)(fns: LinearFn[AT, CustomConnective]): CustomConnective

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
        @implicitNotFound(ErrorMsg.compositionForAllFailed)
        evForAll: CheckForAllMultiplicity[ForAllM, AT, RT, RT],
        @implicitNotFound(ErrorMsg.compositionForEachFailed)
        evForEach: CheckForEachMultiplicity[ForEachM, AT, DT, RT, RT]
      ): LinearFnBuilder[M, AT, ComposedConnective[RT, DT, ForEachM, ForAllM]] with
        def execute(args: AT)(fns: LinearFn[AT, ComposedConnective[RT, DT, ForEachM, ForAllM]]): ComposedConnective[RT, DT, ForEachM, ForAllM] =
          val argsRefs = (0 until args.size).map(i => makeRestrictedRef(() => args.productElement(i).asInstanceOf[Any])).toArray
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
    def apply[M <: Multiplicity, AT <: Tuple, RT <: Tuple, DT <: Tuple, RQT <: Tuple](
      multiplicity: M
    )(args: AT)(fns: LinearFn[AT, RQT])(
      using
        ev1: RT =:= ExtractResultTypes[RQT],
        ev1b: DT =:= ExtractDependencyTypes[RQT],
        ev3: RQT =:= ToRestricted[RT, DT],
        @implicitNotFound(ErrorMsg.compositionForAllFailed)
        evForAllRelevant: ExpectedResult[AT] <:< ActualResult[RQT],
        @implicitNotFound(ErrorMsg.compositionForEachFailed)
        evForAllAffine: AllArgsUsedAtMostOnceTotal[GenerateIndices[0, Tuple.Size[AT]], RQT] =:= true
    ): ExtractResultTypes[RQT] = {
      val argsRefs = args.toArray.map(a => makeRestrictedRef(() => a))
      val refsTuple = Tuple.fromArray(argsRefs).asInstanceOf[ToRestrictedRef[AT]]
      val exec = fns(refsTuple)
      tupleExecute(exec).asInstanceOf[ExtractResultTypes[RQT]]
    }

