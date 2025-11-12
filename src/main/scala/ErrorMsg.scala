package linearfn

object ErrorMsg:
  inline val substructuralContstraintFailed = "Substructural constraint not satisfied: ${VC}, ${HC}"
  inline val invalidResultTypes = "Cannot extract result types from function return type"
  inline val invalidDependencyTypes = "Cannot extract dependency types from function return type"
  inline val invalidConsumptionTypes = "Cannot extract consumption types from function return type"
  inline val invalidRestrictedTypes = "Function return type must be a tuple of Restricted types ${RQT}"
  inline val verticalConstraintFailed = "Constraint not satisfied: ${VC}"
  inline val horizontalRelevanceFailed = "Relevance constraint not satisfied: ${HC}"
  inline val horizontalAffineFailed = "Affine constraint not satisfied: ${HC}"
  inline val strictFnFailed = "Number of actual arguments must match the number of elements returned by fns:  ${AT} => ${AT}"
  
