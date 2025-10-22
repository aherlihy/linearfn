package linearfn

/**
 * Example demonstrating @unrestricted annotation.
 * Unrestricted parameters can be used multiple times without failing linearity checks.
 */
@ops
case class UnrestrictedExample(value: String):
  /** Method with one tracked and one @unrestricted parameter */
  def combine(tracked: UnrestrictedExample, @unrestricted config: String): UnrestrictedExample =
    UnrestrictedExample(s"$value + ${tracked.value} (config: $config)")

  /** Method with all @unrestricted parameters */
  def withConfig(@unrestricted prefix: String, @unrestricted suffix: String): UnrestrictedExample =
    UnrestrictedExample(s"$prefix$value$suffix")

  /** Method with mixed: tracked, unrestricted, tracked */
  def complexOp(
    first: UnrestrictedExample,
    @unrestricted config: String,
    second: UnrestrictedExample
  ): UnrestrictedExample =
    UnrestrictedExample(s"$value + ${first.value} + ${second.value} (config: $config)")
