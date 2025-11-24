package test

import restrictedfn.{ErrorMsg, Multiplicity }
import restrictedfn.RestrictedSelectable.{given, *}
object TestUtils:
  val compositionForAllFailed = ErrorMsg.compositionForAllFailed.split(":", 2).head
  val compositionForEachFailed = ErrorMsg.compositionForEachFailed.split(":", 2).head
  val fixedPointReturnLengthFailed = "fixedPoint requires same number of arguments and returns"
  val fixedPointReturnTypesFailed = "fixedPoint requires return types to match argument types"
  val missingField = "is not a member of"
  val noGivenInstance = "No given instance of type"
  val forAll = "ForAll"
  val forEach = "ForEach"
  val affine = "Multiplicity.Affine"
  val relevant = "Multiplicity.Relevant"
  val linear = "Multiplicity.Linear"

type ForAllLinearConnective[RT <: Tuple] = CustomConnective[RT, Multiplicity.Unrestricted, Multiplicity.Linear]
object ForAllLinearConnective:
  def apply[RT <: Tuple]
  (values: RT): ForAllLinearConnective[RT] =
    CustomConnective[RT, Multiplicity.Unrestricted, Multiplicity.Linear](values)

type ForAllAffineConnective[RT <: Tuple] = CustomConnective[RT, Multiplicity.Unrestricted, Multiplicity.Affine]
object ForAllAffineConnective:
  def apply[RT <: Tuple]
  (values: RT): ForAllAffineConnective[RT] =
    CustomConnective[RT, Multiplicity.Unrestricted, Multiplicity.Affine](values)

type ForAllRelevantConnective[RT <: Tuple] = CustomConnective[RT, Multiplicity.Unrestricted, Multiplicity.Relevant]
object ForAllRelevantConnective:
  def apply[RT <: Tuple]
  (values: RT): ForAllRelevantConnective[RT] =
    CustomConnective[RT, Multiplicity.Unrestricted, Multiplicity.Relevant](values)

