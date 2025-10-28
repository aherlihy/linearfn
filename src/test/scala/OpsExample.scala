package test

import linearfn.{ops, unrestricted, repeatable}

/**
 * Comprehensive example class for testing @ops annotation and extension generation.
 *
 * Demonstrates all method signature patterns:
 * - Methods with no arguments
 * - Methods with single/multiple tracked arguments
 * - Methods with primitive arguments (tracked by default via implicit conversion)
 * - Methods with product (case class) arguments
 * - Methods with @unrestricted parameters (not tracked)
 * - Methods with mixed tracked and @unrestricted parameters
 * - Methods with different return types
 */
@ops
case class OpsExample(name: String, value: String = ""):
  /** Method with no arguments */
  @repeatable
  def noArg(): String = s"Hello, I'm $name"

  /** Method with single tracked product argument */
  @repeatable
  def singleRestrictedProductArg(other: OpsExample): OpsExample =
    OpsExample(s"${this.name} & ${other.name}", s"${this.value}+${other.value}")

  /** Method with single tracked primitive argument */
  @repeatable
  def singleRestrictedPrimitiveArg(newName: String): OpsExample =
    OpsExample(newName, value)

  /** Method with multiple tracked primitive arguments */
  @repeatable
  def multipleRestrictedPrimitiveArgs(prefix: String, suffix: String): String =
    s"$prefix $name $suffix"

  /** Method with tracked product arg, then tracked primitive arg */
  @repeatable
  def restrictedProductArg_RestrictedPrimitiveArg(other: OpsExample, greeting: String): String =
    s"$greeting, I'm ${this.name} and this is ${other.name}"

  /** Method with tracked primitive arg, then tracked product arg */
  @repeatable
  def restrictedPrimitiveArg_RestrictedProductArg(label: String, other: OpsExample): String =
    s"$label: ${this.name} vs ${other.name}"

  /** Method with no arguments returning primitive from field access */
  @repeatable
  def noArgFieldAccess(): String =
    name.toUpperCase

  /** Method with different product type (Helper) */
  @repeatable
  def singleRestrictedDifferentProductArg(helper: Helper): String =
    s"$name says hello to ${helper.species}"

  /** Method with different product type and primitive */
  @repeatable
  def restrictedDifferentProductArg_RestrictedPrimitiveArg(helper: Helper, prefix: String): String =
    s"$prefix, I'm $name and this is a ${helper.species} that says ${helper.sound}"

  /** Method with tracked product arg and @unrestricted product arg */
  @repeatable
  def restrictedProductArg_UnrestrictedProductArg(helper1: Helper, @unrestricted helper2: Helper): String =
    s"$name compares ${helper1.species} (tracked) with ${helper2.species} (not tracked)"

  /** Method with multiple tracked product args and @unrestricted product arg */
  @repeatable
  def restrictedProductArg_restrictedProductArg_UnrestrictedProductArg(
    helper1: Helper,
    helper2: Helper,
    @unrestricted helper3: Helper
  ): String =
    s"$name compares ${helper1.species} (tracked) with ${helper2.species} (tracked) with ${helper3.species} (not tracked)"

  /** Method with tracked product arg and @unrestricted primitive arg */
  @repeatable
  def restrictedProductArg_UnrestrictedPrimitiveArg(other: OpsExample, @unrestricted config: String): OpsExample =
    OpsExample(s"$name + ${other.name} (config: $config)", s"${this.value}+${other.value}")

  /** Method with all @unrestricted parameters */
  @repeatable
  def allUnrestrictedArgs(@unrestricted prefix: String, @unrestricted suffix: String): OpsExample =
    OpsExample(s"$prefix$name$suffix", value)

  /** Method with mixed: tracked, @unrestricted, tracked */
  @repeatable
  def mixedTrackedAndUnrestricted(
    first: OpsExample,
    @unrestricted config: String,
    second: OpsExample
  ): OpsExample =
    OpsExample(s"$name + ${first.name} + ${second.name} (config: $config)", s"$value+${first.value}+${second.value}")

/**
 * Helper class for testing methods with different product types.
 */
case class Helper(species: String, sound: String)
