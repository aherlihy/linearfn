package linearfn

import scala.annotation.StaticAnnotation

/**
 * Marks a parameter as NOT requiring a Restricted wrapper in generated extension methods.
 *
 * By default, all non-primitive parameters are automatically Restricted (tracked).
 * Use @unrestricted to opt-out of tracking for specific parameters.
 *
 * Example:
 * ```
 * def compareAnimals(animal1: Animal, @unrestricted animal2: Animal): Boolean =
 *   animal1.species == animal2.species
 * ```
 *
 * This generates:
 * ```
 * def compareAnimals[D2 <: Tuple](animal1: Animal ~ D2, animal2: Animal): Boolean ~ (Tuple.Concat[D2, D]) =
 *   p.stageCall[Boolean, Tuple.Concat[D2, D]]("compareAnimals", (animal1, animal2))
 * ```
 *
 * animal1 is tracked (Restricted), animal2 is not (just a regular Animal value).
 */
class unrestricted extends StaticAnnotation
