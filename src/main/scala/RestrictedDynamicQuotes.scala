package restrictedfn

import scala.quoted.*
import scala.language.dynamics
import scala.annotation.implicitNotFound
import Utils.*

/**
 * Use Dynamic with quotes/splices to build AST directly.
 * No runtime reflection - the compiler type-checks field/method access.
 */
object RestrictedDynamicQuotes extends RestrictedFnBase:

  // Implementation-specific Restricted trait - extends RestrictedBase
  trait Restricted[A, D <: Tuple] extends RestrictedBase[A, D], Dynamic:
    // Use inline with transparent return type for precise type inference
    transparent inline def selectDynamic(inline name: String): Any =
      ${ selectDynamicQuote[A, D]('this, 'name) }

    transparent inline def applyDynamic(inline name: String)(inline args: Any*): Any =
      ${ applyDynamicQuote[A, D]('this, 'name, 'args) }

    def execute(): A

  // Implementation-specific RestrictedRef
  object Restricted:
    case class RestrictedRef[A, D <: Tuple](val fn: () => A) extends Restricted[A, D]:
      def execute(): A = fn()

  // Implement abstract methods from RestrictedFnBase
  protected def makeRestrictedRef[A, D <: Tuple](fn: () => A): Restricted[A, D] =
    Restricted.RestrictedRef(fn)

  // Inline quote implementation for selectDynamic - builds Select AST directly
  private def selectDynamicQuote[A: Type, D <: Tuple: Type](
    self: Expr[Restricted[A, D]],
    name: Expr[String]
  )(using Quotes): Expr[Any] =
    import quotes.reflect.*

    val fieldName = name.valueOrAbort

    // Build the Select AST
    val linearRefExpr = '{ $self.asInstanceOf[Restricted.RestrictedRef[A, D]] }
    val fnCall = '{ $linearRefExpr.fn() }
    val selectTree = Select.unique(fnCall.asTerm, fieldName)

    // Extract the field type from the Select tree
    selectTree.tpe.asType match
      case '[fieldType] =>
        val resultExpr = selectTree.asExprOf[fieldType]
        '{
          Restricted.RestrictedRef[fieldType, D](() => $resultExpr): Restricted[fieldType, D]
        }.asExprOf[Restricted[fieldType, D]]

  // Inline quote implementation for applyDynamic - builds Apply AST directly
  private def applyDynamicQuote[A: Type, D <: Tuple: Type](
    self: Expr[Restricted[A, D]],
    name: Expr[String],
    args: Expr[Seq[Any]]
  )(using Quotes): Expr[Any] =
    import quotes.reflect.*

    val methodName = name.valueOrAbort

    // Extract args and compute dependency type
    val argExprs = args.asTerm match {
      case Inlined(_, _, Typed(Repeated(argList, _), _)) => argList
      case Typed(Repeated(argList, _), _) => argList
      case _ => Nil
    }

    var currentDeps = TypeRepr.of[D]
    for argExpr <- argExprs do
      argExpr.tpe.dealias.widen match {
        case AppliedType(restrictedType, List(_, dArg))
          if restrictedType.typeSymbol.fullName == "restrictedfn.RestrictedDynamicQuotes$.Restricted" =>
          currentDeps = TypeRepr.of[Tuple.Concat].appliedTo(List(dArg, currentDeps))
        case _ => ()
      }

    // Build AST
    val linearRefExpr = '{ $self.asInstanceOf[Restricted.RestrictedRef[A, D]] }
    val fnCall = '{ $linearRefExpr.fn() }
    val argsExpr = '{
      $args.map {
        case r: Restricted[_, _] => r.execute()
        case x => x
      }
    }

    val applyTree = if argExprs.isEmpty then
      Apply(Select.unique(fnCall.asTerm, methodName), Nil)
    else
      val argTerms = '{ $argsExpr.toList }.asTerm match {
        case Inlined(_, _, Typed(Repeated(argList, _), _)) => argList
        case Typed(Repeated(argList, _), _) => argList
        case term => List(term)
      }
      Apply(Select.unique(fnCall.asTerm, methodName), argTerms)

    // Extract the return type from the Apply tree
    currentDeps.dealias.simplified.asType match {
      case '[newD] =>
        applyTree.tpe.asType match {
          case '[rt] =>
            val resultExpr = applyTree.asExprOf[rt]
            '{
              Restricted.RestrictedRef[rt, newD & Tuple](() => $resultExpr): Restricted[rt, newD & Tuple]
            }.asExprOf[Restricted[rt, newD & Tuple]]
        }
    }

end RestrictedDynamicQuotes
