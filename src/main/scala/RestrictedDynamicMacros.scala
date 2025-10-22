package linearfn

import scala.language.implicitConversions
import scala.language.dynamics
import NamedTuple.{AnyNamedTuple, NamedTuple}
import scala.annotation.{implicitNotFound, targetName}
import scala.quoted.*
import scala.compiletime.summonInline
import Utils.*

/**
 * Use Dynamic but with macros for type safety.
 */
object RestrictedDynamicMacros extends LinearFnBase:

  // Implementation-specific Restricted trait
  trait Restricted[A, D <: Tuple] extends Dynamic:
    // Macro-based implementations for compile-time verification
    transparent inline def selectDynamic(inline name: String): Any =
      ${ RestrictedMacros.selectDynamicImpl[A, D]('this, 'name) }

    transparent inline def applyDynamic(inline name: String)(inline args: Any*): Any =
      ${ RestrictedMacros.applyDynamicImpl[A, D]('this, 'name, 'args) }

    def execute(): A

  // Implementation-specific LinearRef
  object Restricted:
    case class LinearRef[A, D <: Tuple](val fn: () => A) extends Restricted[A, D]:
      def execute(): A = fn()

  // Implement abstract methods from LinearFnBase
  protected def makeLinearRef[A, D <: Tuple](fn: () => A): Restricted[A, D] =
    Restricted.LinearRef(fn)

  protected def executeRestricted[A, D <: Tuple](r: Restricted[A, D]): A =
    r.execute()