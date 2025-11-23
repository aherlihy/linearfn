package linearfn

import linearfn.Utils.*
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
 */

trait RestrictedBase[A, D <: Tuple]:
  def execute(): A

abstract class RestrictedFnBase:

  /**
   * Restricted: Type that wraps restricted function arguments
   */
  type Restricted[A, D <: Tuple] <: RestrictedBase[A, D]

  /**
   * ComposedConnective: The result of composing Restricted values with a CustomConnective.
   */
  case class ComposedConnective[
    RQT <: Tuple,
    ForEachM <: Multiplicity,
    ForAllM <: Multiplicity
  ](
    values: RQT
  ) extends RestrictedBase[ExtractResultTypes[RQT], ExtractDependencyTypes[RQT]]:
    def execute(): ExtractResultTypes[RQT] =
      tupleExecute(values).asInstanceOf[ExtractResultTypes[RQT]]

  // Factory method for creating RestrictedRef instances
  protected def makeRestrictedRef[A, D <: Tuple](fn: () => A): Restricted[A, D]

  // Unwrap a Restricted value
  protected def executeRestricted[A, D <: Tuple](r: Restricted[A, D]): A =
    r.execute()

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

  // Construct restricted types from arguments
  // Converts argument to Restricted types with single-element dependency tuples
  // Each argument gets a unique index: arg0 → (0), arg1 → (1), etc.
  type ToRestrictedRef[AT <: Tuple] = Tuple.Map[ZipWithIndex[AT], [T] =>> T match
    case (elem, index) => Restricted[elem, Tuple1[index]]
  ]

  // If any return has duplicates (true), compilation fails
  // Example: (a, a) has duplicates → false; (a, b) has no duplicates → true
  type InverseMapDeps[RQT <: Tuple] <: Tuple = RQT match {
    case EmptyTuple => EmptyTuple
    case h *: t => LiftInnerType[h] match
      case (_, d) => HasDuplicate[d] *: InverseMapDeps[t]
  }

  // Used to construct the Restricted types returned by the function
  // RT is the return tuple, DT is the tuple of dependency tuples
  type ToRestricted[RT <: Tuple, DT <: Tuple] =
    Tuple.Map[Tuple.Zip[RT, DT], [T] =>> ConstructRestricted[T]]

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

//  // Recursively extract dependencies from nested containers
//  // Note: This uses a different recursion pattern than LiftInnerType
  type ExtractDependencies[D] <: Tuple = D match
    case Restricted[_, d] => d
    case List[inner] => ExtractDependencies[inner]
    case Option[inner] => ExtractDependencies[inner]
    case Vector[inner] => ExtractDependencies[inner]

  // Check that all arguments are used somewhere
  // ExpectedResult: Union of all argument indices {0, 1, 2, ...}
  // ActualResult: Union of all dependencies actually used across all returns
  // Constraint: ExpectedResult <:< ActualResult ensures every arg appears at least once
  // Example: args=(a,b), returns=(a,a) → ActualResult={0}, ExpectedResult={0,1} → FAIL (b unused)
  type ExpectedResult[AT <: Tuple] = Tuple.Union[GenerateIndices[0, Tuple.Size[AT]]]
  type ActualResult[RQT <: Tuple] = Tuple.Union[Tuple.FlatMap[RQT, ExtractDependencies]]

  // Extract the wrapped type of a Restricted type
  type ExtractResultTypes[RQT <: Tuple] <: Tuple = RQT match
    case EmptyTuple => EmptyTuple
    case h *: tail => LiftInnerType[h] match
      case (a, _) => a *: ExtractResultTypes[tail]

  // Extract the dependencies of a Restricted type
  type ExtractDependencyTypes[RQT <: Tuple] <: Tuple = RQT match
    case EmptyTuple => EmptyTuple
    case h *: tail => LiftInnerType[h] match
      case (_, d) => d *: ExtractDependencyTypes[tail]

  // Combine dependencies of a Restricted type A and a dependency tuple D
  type CollateDeps[A, D <: Tuple] <: Tuple = A match
    case Restricted[a, d] => Tuple.Concat[d, D]
    case _ => D

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
  type CheckForEachAffine[RQT <: Tuple] =
    InverseMapDeps[RQT] =:= ExtractDependencyTypes[RQT]  // This already checks for duplicates

  // Helper: Check for-each-relevant (all args in every return)
  type CheckForEachRelevant[AT <: Tuple, RQT <: Tuple] =
    AllReturnsContainAllArgs[GenerateIndices[0, Tuple.Size[AT]], RQT]

  type AllReturnsContainAllArgs[Indices <: Tuple, RQT <: Tuple] <: Boolean = RQT match
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
      Tuple.Contains[D, idx] match
        case true => AllIndicesInDeps[rest, D]
        case false => false

  // Given instance for true - used when Unrestricted multiplicity bypasses constraints
  given trueEvidence: true = true

  // Given instance for tuple constraints - used when CheckForEachLinear returns a tuple
  given tupleEvidence[A, B](using A, B): (A, B) = (summon[A], summon[B])

  // Helper: Check for-all-linear (each arg used exactly once total = relevant + affine)
  type CheckForAllLinear[AT <: Tuple, RQT <: Tuple] =
    (CheckForAllRelevant[AT, RQT], CheckForAllAffine[AT, RQT])

  // Check ForAll multiplicity constraint (across all returns)
  type CheckForAllMultiplicity[
    ForAllM <: Multiplicity,
    AT <: Tuple,
    RQT <: Tuple
  ] = ForAllM match
    case Multiplicity.Linear => CheckForAllLinear[AT, RQT]    // Must appear exactly once total (relevant + affine)
    case Multiplicity.Affine => CheckForAllAffine[AT, RQT]    // Can appear at most once total
    case Multiplicity.Relevant => CheckForAllRelevant[AT, RQT] // Must appear at least once total
    case Multiplicity.Unrestricted => true                     // No constraint (always satisfied)

  // Check ForEach multiplicity constraint (per individual return)
  type CheckForEachMultiplicity[
    ForEachM <: Multiplicity,
    AT <: Tuple,
    RQT <: Tuple
  ] = ForEachM match
    case Multiplicity.Linear => CheckForEachLinear[AT, RQT]       // Each return: exactly once (relevant + affine)
    case Multiplicity.Affine => CheckForEachAffine[RQT]           // Each return: at most once
    case Multiplicity.Relevant => CheckForEachRelevant[AT, RQT]       // Each return: at least once
    case Multiplicity.Unrestricted => true                            // No constraint (always satisfied)

  type CheckForEachLinear[AT <: Tuple, RQT <: Tuple] =
    (CheckForEachRelevant[AT, RQT], CheckForEachAffine[RQT])

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

    type ExtractReturnType[CustomConnective] = CustomConnective match
      case ComposedConnective[rqt, forEachM, forAllM] =>
        ExtractResultTypes[rqt]

    trait LinearFnBuilder[M <: Multiplicity, AT <: Tuple, CustomConnective]:
      def execute(args: AT)(fns: LinearFn[AT, CustomConnective]): ExtractReturnType[CustomConnective]

    object LinearFnBuilder:
      // Builder for ComposedConnective - enforces custom connective constraints
      given connectiveBuilder[
        M <: Multiplicity,
        AT <: Tuple,    // args types
        RQT <: Tuple,   // return types (restricted)
        ForEachM <: Multiplicity,
        ForAllM <: Multiplicity
      ](using
        @implicitNotFound(ErrorMsg.compositionForAllFailed)
        evForAll: CheckForAllMultiplicity[ForAllM, AT, RQT],
        @implicitNotFound(ErrorMsg.compositionForEachFailed)
        evForEach: CheckForEachMultiplicity[ForEachM, AT, RQT],
//        @implicitNotFound("DEBUG: ${RQT}") debug: RQT =:= false
      ): LinearFnBuilder[M, AT, ComposedConnective[RQT, ForEachM, ForAllM]] with
        def execute(args: AT)(fns: LinearFn[AT, ComposedConnective[RQT, ForEachM, ForAllM]]): ExtractResultTypes[RQT] =
          val restrictedRefs = (0 until args.size).map(i => makeRestrictedRef(() => args.productElement(i).asInstanceOf[Any])).toArray
          val restrictedRefsTuple= Tuple.fromArray(restrictedRefs).asInstanceOf[ToRestrictedRef[AT]]
          val resultConnective = fns(restrictedRefsTuple)
          val evaluated = resultConnective.execute()
          evaluated.asInstanceOf[ExtractResultTypes[RQT]]

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

