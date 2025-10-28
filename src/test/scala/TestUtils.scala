package test

object TestUtils:
  val affineMsg = "is the query affine?"
  val linearMsg = "must be linear"
  val argsMsg = "Linear functions must have the same number of argument and return types and the return types must be Restricted"
  val consumptionAtMostOneMsg = "All return values must have consumption state of length 0 or 1"
  val consumptionExactlyOneMsg = "All return values must be consumed (consumption state length must be exactly 1)"
  val restrictedTypesMsg = "Failed to match restricted types"