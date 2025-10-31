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
object RestrictedDynamicMacros extends RestrictedFnBase:

  // Implementation-specific Restricted trait - extends RestrictedBase
  trait Restricted[A, D <: Tuple, C <: Tuple] extends RestrictedBase[A, D, C], Dynamic:
    // Macro-based implementations for compile-time verification
    transparent inline def selectDynamic(inline name: String): Any =
      ${ RestrictedMacros.selectDynamicImpl[A, D, C]('this, 'name) }

    transparent inline def applyDynamic(inline name: String)(inline args: Any*): Any =
      ${ RestrictedMacros.applyDynamicImpl[A, D, C]('this, 'name, 'args) }

    def execute(): A

  // Implementation-specific RestrictedRef
  object Restricted:
    case class RestrictedRef[A, D <: Tuple, C <: Tuple](val fn: () => A) extends Restricted[A, D, C]:
      def execute(): A = fn()

  // Implement abstract methods from RestrictedFnBase
  protected def makeRestrictedRef[A, D <: Tuple, C <: Tuple](fn: () => A): Restricted[A, D, C] =
    Restricted.RestrictedRef(fn)