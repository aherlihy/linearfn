package restrictedfn

import scala.annotation.StaticAnnotation

/**
 * Marker annotation for classes that should have extension methods generated.
 *
 * This is a simple marker annotation (NOT a macro annotation). The actual
 * extension method generation is done by the sbt source generator that scans
 * for classes annotated with @ops.
 *
 * Usage:
 * {{{
 * @ops
 * case class Person(name: String, age: Int):
 *   def greet(): String = s"Hello, I'm $name"
 *   def combine(other: Person): Person = ???
 * }}}
 *
 * During compilation, the sbt source generator (OpsExtensionGenerator.scala)
 * scans for @ops annotations and generates extension methods in:
 * `target/scala-3.x.x/src_managed/main/{ClassName}Extensions.scala`
 *
 * Example generated extensions:
 * {{{
 * object PersonOps:
 *   extension [D <: Tuple](p: RestrictedSelectable.Restricted[Person, D])
 *     def greet(): RestrictedSelectable.Restricted[String, D] =
 *       p.stageCall[String, D]("greet", EmptyTuple)
 *
 *     def combine[D2 <: Tuple](other: RestrictedSelectable.Restricted[Person, D2]):
 *       RestrictedSelectable.Restricted[Person, Tuple.Concat[D2, D]] =
 *       p.stageCall[Person, Tuple.Concat[D2, D]]("combine", Tuple1(other))
 * }}}
 *
 * @see OpsExtensionGenerator for the code generation logic
 */
class ops extends StaticAnnotation

/**
 * Parameter-level annotation for function parameters where the function's return
 * type should be wrapped in Restricted.
 *
 * IMPORTANT RESTRICTION: @restrictedReturn can ONLY be used on single-parameter
 * function types (A => B). It cannot be used on:
 * - Non-function parameters
 * - Multi-parameter functions like (A, B) => C
 *
 * When `@restrictedReturn` is applied to a function parameter:
 * - The function itself is NOT wrapped in Restricted
 * - The function's return type IS wrapped in Restricted
 * - Dependencies are tracked from the function's return value
 *
 * This is useful for higher-order functions where you want to track what the
 * function returns, but not the function itself.
 *
 * Valid usage:
 * {{{
 * @ops
 * class Query[A]:
 *   // Regular parameter - entire parameter wrapped (default behavior)
 *   def union(that: Query[A]): Query[A]
 *
 *   // Function parameter with @restrictedReturn - only return type wrapped
 *   def flatMap[B](@restrictedReturn f: A => Query[B]): Query[B]
 *
 *   // Completely unrestricted function
 *   def map[B](@unrestricted f: A => B): Query[B]
 * }}}
 *
 * Generated extensions:
 * {{{
 * // union: entire parameter wrapped (default behavior)
 * def union[D1 <: Tuple](
 *   that: Restricted[Query[A], D1]
 * ): Restricted[Query[A], Tuple.Concat[D1, D]]
 *
 * // flatMap: function not wrapped, but Query[B] is
 * def flatMap[B, D1 <: Tuple](
 *   f: A => Restricted[Query[B], D1]
 * ): Restricted[Query[B], Tuple.Concat[D1, D]]
 *
 * // map: nothing wrapped (due to @unrestricted on parameter)
 * def map[B](f: A => B): Restricted[Query[B], D]
 * }}}
 *
 * Invalid usage (will cause build-time errors):
 * {{{
 * @ops
 * class BadExamples[A]:
 *   // ERROR: @restrictedReturn on non-function parameter
 *   def withOption(@restrictedReturn opt: Option[A]): A
 *
 *   // ERROR: @restrictedReturn on multi-parameter function
 *   def withMultiParam(@restrictedReturn f: (A, B) => C): C
 * }}}
 */
class restrictedReturn extends StaticAnnotation

/**
 * Marks a parameter as requiring a Restricted wrapper in generated extension methods.
 *
 * NOTE: This is the default behavior for all non-primitive parameters.
 * This annotation is provided for explicitness and documentation purposes.
 *
 * By default, all non-primitive parameters are automatically Restricted (tracked).
 * Use @restricted to explicitly document that a parameter should be tracked.
 * Use @unrestricted to opt-out of tracking for specific parameters.
 *
 * Example:
 * ```
 * @ops
 * class Query[A]:
 *   // Explicitly marked as restricted (same as default)
 *   def unionAll(@restricted that: Query[A]): Query[A] = ???
 *
 *   // Default behavior (implicitly restricted)
 *   def union(that: Query[A]): Query[A] = ???
 *
 *   // Explicitly opt-out of tracking
 *   def withConstant(@unrestricted value: A): Query[A] = ???
 * ```
 *
 * All three parameter annotation behaviors:
 * - @restricted: Parameter is wrapped in Restricted (explicit, same as default)
 * - (no annotation): Parameter is wrapped in Restricted (default)
 * - @unrestricted: Parameter is NOT wrapped in Restricted (opt-out)
 */
class restricted extends StaticAnnotation

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
