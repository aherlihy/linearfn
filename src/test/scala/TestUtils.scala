package test

import linearfn.ErrorMsg

object TestUtils:
  val substructuralConstraintFailed = ErrorMsg.substructuralContstraintFailed.split(":", 2).head
  val multiplicityConstraintFailed = ErrorMsg.multiplicityConstraintFailed.split(":", 2).head
  val compositionForAllFailed = ErrorMsg.compositionForAllFailed.split(":", 2).head
  val compositionForEachFailed = ErrorMsg.compositionForEachFailed.split(":", 2).head
  val fixedPointReturnLengthFailed = ErrorMsg.fixedPointReturnLengthFailed.split(":", 2).head
  val fixedPointReturnTypesFailed = ErrorMsg.fixedPointReturnTypesFailed.split(":", 2).head
  val typeErrorStr = "Expected"
  val missingTypeClassMsg = "Cannot prove"
  val missingField = "is not a member of"
  val otherError = "match type could not be fully reduced"
  val noGivenInstance = "No given instance of type"

