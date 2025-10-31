package linearfn

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
 * Marker annotation for methods that can be called multiple times on the same value.
 *
 * When a method is annotated with @repeatable, the generated extension method:
 * - Can only be called on unconsumed values (C = EmptyTuple)
 * - Returns an unconsumed value (C = EmptyTuple)
 *
 * This allows the method to be called multiple times in a chain without consuming
 * the receiver.
 *
 * Usage:
 * {{{
 * @ops
 * case class MArray[A](private val buf: Array[A]):
 *   @repeatable  // Can be called multiple times
 *   def write(i: Int, a: A): MArray[A] = { buf(i) = a; this }
 *
 *   // Default behavior: consumes the array
 *   def freeze(): Array[A] = buf.clone()
 * }}}
 *
 * The generated extension for write():
 * {{{
 * extension [D <: Tuple](p: Restricted[MArray[A], D, EmptyTuple])
 *   def write(...): Restricted[MArray[A], ..., EmptyTuple] = ...
 * }}}
 *
 * @repeatable and @unconsumed are mutually exclusive.
 */
class repeatable extends StaticAnnotation

/**
 * Marker annotation for methods that consume their receiver.
 *
 * NOTE: As of the current version, the default behavior (no annotation) is the same
 * as @consumed. This annotation is provided for explicitness.
 *
 * When a method is annotated with @consumed (or has no annotation), the generated
 * extension method:
 * - Can only be called on unconsumed values (C = EmptyTuple)
 * - Returns a consumed value (C = Tuple1[true])
 *
 * Usage:
 * {{{
 * @ops
 * case class MArray[A](private val buf: Array[A]):
 *   @repeatable
 *   def write(i: Int, a: A): MArray[A] = { buf(i) = a; this }
 *
 *   @consumed  // Explicit annotation (same as default)
 *   def freeze(): Array[A] = buf.clone()
 * }}}
 *
 * The generated extension for freeze():
 * {{{
 * extension [D <: Tuple](p: Restricted[MArray[A], D, EmptyTuple])
 *   def freeze(): Restricted[Array[A], D, Tuple1[true]] = ...
 * }}}
 *
 * This allows the type system to track that freeze() consumes the MArray,
 * preventing further use after calling freeze().
 *
 * @consumed and @unconsumed are mutually exclusive.
 */
class consumed extends StaticAnnotation

/**
 * Marker annotation for methods that can be called on consumed or unconsumed values.
 *
 * When a method is annotated with @unconsumed, the generated extension method:
 * - Can be called on values in any consumption state (C <: Tuple)
 * - Preserves the exact consumption state of the receiver
 *
 * Usage:
 * {{{
 * @ops
 * case class MArray[A](private val buf: Array[A]):
 *   @unconsumed  // Can be called on consumed or unconsumed arrays
 *   def size(): Int = buf.length
 * }}}
 *
 * The generated extension for size():
 * {{{
 * extension [D <: Tuple, C <: Tuple](p: Restricted[MArray[A], D, C])
 *   def size(): Restricted[Int, D, C] = ...
 * }}}
 *
 * This allows calling query methods on both consumed and unconsumed values,
 * preserving the consumption state.
 *
 * @consumed and @unconsumed are mutually exclusive.
 */
class unconsumed extends StaticAnnotation

/**
 * Parameter-level annotation for function parameters where the function's return
 * type should be wrapped in Restricted.
 *
 * IMPORTANT RESTRICTION: @restrictedFn can ONLY be used on single-parameter
 * function types (A => B). It cannot be used on:
 * - Non-function parameters
 * - Multi-parameter functions like (A, B) => C
 *
 * When `@restrictedFn` is applied to a function parameter:
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
 *   // Function parameter with @restrictedFn - only return type wrapped
 *   def flatMap[B](@restrictedFn f: A => Query[B]): Query[B]
 *
 *   // Completely unrestricted function
 *   def map[B](@unrestricted f: A => B): Query[B]
 * }}}
 *
 * Generated extensions:
 * {{{
 * // union: entire parameter wrapped (default behavior)
 * def union[D1 <: Tuple, C1 <: Tuple](
 *   that: Restricted[Query[A], D1, C1]
 * ): Restricted[Query[A], Tuple.Concat[D1, D], EmptyTuple]
 *
 * // flatMap: function not wrapped, but Query[B] is
 * def flatMap[B, D1 <: Tuple, C1 <: Tuple](
 *   f: A => Restricted[Query[B], D1, C1]
 * ): Restricted[Query[B], Tuple.Concat[D1, D], EmptyTuple]
 *
 * // map: nothing wrapped (due to @unrestricted on parameter)
 * def map[B](f: A => B): Restricted[Query[B], D, EmptyTuple]
 * }}}
 *
 * Invalid usage (will cause build-time errors):
 * {{{
 * @ops
 * class BadExamples[A]:
 *   // ERROR: @restrictedFn on non-function parameter
 *   def withOption(@restrictedFn opt: Option[A]): A
 *
 *   // ERROR: @restrictedFn on multi-parameter function
 *   def withMultiParam(@restrictedFn f: (A, B) => C): C
 * }}}
 */
class restrictedFn extends StaticAnnotation

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
