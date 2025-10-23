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
