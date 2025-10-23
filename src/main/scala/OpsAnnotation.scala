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
 * Marker annotation for methods that consume their receiver.
 *
 * When a method is annotated with @consumed, the generated extension method:
 * - Can only be called on unconsumed values (C = EmptyTuple)
 * - Returns a consumed value (C = Tuple1[true])
 *
 * Usage:
 * {{{
 * @ops
 * case class MArray[A](private val buf: Array[A]):
 *   def write(i: Int, a: A): MArray[A] = { buf(i) = a; this }
 *
 *   @consumed  // This method consumes the array
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
