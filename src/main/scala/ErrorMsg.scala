package restrictedfn

object ErrorMsg:
  // Core constraints
  inline val compositionForAllFailed = "Composition ForAll multiplicity constraint not satisfied: ${ForAllM}"
  inline val compositionForEachFailed = "Composition ForEach multiplicity constraint not satisfied: ${ForEachM}"

