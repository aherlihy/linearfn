package test

import linearfn.{ErrorMsg, Multiplicity }
import linearfn.RestrictedSelectable.{given, *}
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
  val forAll = "ForAll"
  val forEach = "ForEach"
  val affine = "Multiplicity.Affine"
  val relevant = "Multiplicity.Relevant"
  val linear = "Multiplicity.Linear"

type ForAllLinearConnective[RT <: Tuple] = ComposedConnective[RT, Multiplicity.Unrestricted, Multiplicity.Linear]
object ForAllLinearConnective:
  def apply[RT <: Tuple]
  (values: RT): ForAllLinearConnective[RT] =
    ComposedConnective[RT, Multiplicity.Unrestricted, Multiplicity.Linear](values)

type ForAllAffineConnective[RT <: Tuple] = ComposedConnective[RT, Multiplicity.Unrestricted, Multiplicity.Affine]
object ForAllAffineConnective:
  def apply[RT <: Tuple]
  (values: RT): ForAllAffineConnective[RT] =
    ComposedConnective[RT, Multiplicity.Unrestricted, Multiplicity.Affine](values)

type ForAllRelevantConnective[RT <: Tuple] = ComposedConnective[RT, Multiplicity.Unrestricted, Multiplicity.Relevant]
object ForAllRelevantConnective:
  def apply[RT <: Tuple]
  (values: RT): ForAllRelevantConnective[RT] =
    ComposedConnective[RT, Multiplicity.Unrestricted, Multiplicity.Relevant](values)

