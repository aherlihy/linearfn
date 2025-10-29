package test

import linearfn.ErrorMsg

object TestUtils:
  val typeErrorStr = "Expected"
  val verticalConstraintFailed = ErrorMsg.verticalConstraintFailed.split(":", 2).head
  val horizontalRelevanceFailed = ErrorMsg.horizontalRelevanceFailed.split(":", 2).head
  val horizontalAffineFailed = ErrorMsg.horizontalAffineFailed.split(":", 2).head
  val strictFnFailed = ErrorMsg.strictFnFailed.split(":", 2).head
  val missingTypeClassMsg = "Cannot prove"
  val missingField = "is not a member of"
  val otherError = "match type could not be fully reduced"

