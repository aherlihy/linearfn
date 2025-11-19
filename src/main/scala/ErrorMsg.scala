package linearfn

object ErrorMsg:
  inline val substructuralContstraintFailed = "Substructural constraint not satisfied: ${M}"
  inline val invalidResultTypes = "Cannot extract result types from function return type"
  inline val invalidDependencyTypes = "Cannot extract dependency types from function return type"
  inline val invalidConsumptionTypes = "Cannot extract consumption types from function return type"
  inline val invalidRestrictedTypes = "Function return type must be a tuple of Restricted types ${RQT}"
  inline val multiplicityConstraintFailed = "Multiplicity constraint not satisfied: ${M}"
  inline val compositionForAllFailed = "Composition ForAll multiplicity constraint not satisfied: ${ForAllM}"
  inline val compositionForEachFailed = "Composition ForEach multiplicity constraint not satisfied: ${ForEachM}"
  inline val strictFnFailed = "Number of actual arguments must match the number of elements returned by fns:  ${AT} => ${AT}"
  
