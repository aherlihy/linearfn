package linearfn

object ErrorMsg:
  // Core constraints
  inline val substructuralContstraintFailed = "Substructural constraint not satisfied: ${M}"
  inline val multiplicityConstraintFailed = "Multiplicity constraint not satisfied: ${M}"
  inline val compositionForAllFailed = "Composition ForAll multiplicity constraint not satisfied: ${ForAllM}"
  inline val compositionForEachFailed = "Composition ForEach multiplicity constraint not satisfied: ${ForEachM}"

  // FixedPoint constraints
  inline val fixedPointReturnLengthFailed = "fixedPoint requires same number of arguments and returns"
  inline val fixedPointReturnTypesFailed = "fixedPoint requires return types to match argument types"

