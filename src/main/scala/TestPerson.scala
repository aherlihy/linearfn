package linearfn

/**
 * Comprehensive example class for testing @ops annotation and extension generation.
 *
 * TestPerson demonstrates various method signatures:
 * - Methods with no arguments
 * - Methods with tracked non-primitive arguments
 * - Methods with primitive arguments (all parameters are tracked by default)
 * - Methods with multiple parameters
 * - Methods with @unrestricted parameters
 * - Methods with different return types
 */
@ops
case class TestPerson(name: String, age: Int):
  /** Method with no arguments */
  def noArg(): String = s"Hello, I'm $name"

  /** Method with one tracked non-primitive argument */
  def singleRestrictedProductArg(other: TestPerson): TestPerson =
    TestPerson(s"${this.name} & ${other.name}", this.age + other.age)

  /** Method with primitive argument (tracked by default via implicit conversion) */
  def singleRestrictedPrimitiveArg(newName: String): TestPerson =
    TestPerson(newName, age)

  /** Method with two primitive arguments (both tracked via implicit conversion) */
  def multipleRestrictedPrimitiveArgs(prefix: String, suffix: String): String =
    s"$prefix $name $suffix"

  /** Method with non-primitive first, then primitive (both tracked) */
  def restrictedProductArg_RestrictedPrimitiveArg(other: TestPerson, greeting: String): String =
    s"$greeting, I'm ${this.name} and this is ${other.name}"

  /** Method with primitive first, then non-primitive (both tracked) */
  def restrictedPrimitiveArg_RestrictedProductArg(label: String, other: TestPerson): String =
    s"$label: ${this.name} vs ${other.name}"

  /** Method with no arguments returning primitive from field access */
  def noArgFieldAccess(): String =
    name.toUpperCase

  /** Method with different non-primitive type */
  def singleRestrictedDifferentProductArg(animal: Animal): String =
    s"$name says hello to the ${animal.species}"

  /** Method with different non-primitive and primitive */
  def restrictedDifferentProductArg_RestrictedPrimitiveArg(animal: Animal, prefix: String): String =
    s"$prefix, I'm $name and this is a ${animal.species} that says ${animal.sound}"

  /** Method demonstrating @unrestricted - second Animal is not tracked */
  def restrictedProductArg_UnrestrictedProductArg(animal1: Animal, @unrestricted animal2: Animal): String =
    s"$name compares ${animal1.species} (tracked) with ${animal2.species} (not tracked)"

  def restrictedProductArg_restrictedProductArg_UnrestrictedProductArg(animal1: Animal, animal2: Animal, @unrestricted animal3: Animal): String =
    s"$name compares ${animal1.species} (tracked) with ${animal2.species} (tracked) with ${animal2.species} (not tracked)"

/**
 * Helper class for testing methods with different non-primitive types.
 */
case class Animal(species: String, sound: String)
