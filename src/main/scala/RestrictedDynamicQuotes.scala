package linearfn

import scala.quoted.*
import scala.language.dynamics
import scala.annotation.implicitNotFound
import Utils.*

/**
 * Use Dynamic with quotes/splices to build AST directly.
 * No runtime reflection - the compiler type-checks field/method access.
 */
object RestrictedDynamicQuotes:
  type ToLinearRef[AT <: Tuple] = Tuple.Map[ZipWithIndex[AT], [T] =>> T match
    case (elem, index) => Restricted[elem, Tuple1[index]]]

  type InverseMapDeps[RT <: Tuple] <: Tuple = RT match {
    case Restricted[_, d] *: t => HasDuplicate[d] *: InverseMapDeps[t]
    case EmptyTuple => EmptyTuple
  }

  type ToRestricted[AT <: Tuple, DT <: Tuple] =
    Tuple.Map[Tuple.Zip[AT, DT], [T] =>> ConstructRestricted[T]]

  type ConstructRestricted[T] = T match
    case (a, d) => Restricted[a, d]

  type ExtractDependencies[D] <: Tuple = D match
    case Restricted[_, d] => d

  type ExpectedResult[QT <: Tuple] = Tuple.Union[GenerateIndices[0, Tuple.Size[QT]]]
  type ActualResult[RT <: Tuple] = Tuple.Union[Tuple.FlatMap[RT, ExtractDependencies]]

  type ExtractResultTypes[RQT <: Tuple] <: Tuple = RQT match
    case EmptyTuple => EmptyTuple
    case Restricted[a, _] *: tail => a *: ExtractResultTypes[tail]

  type ExtractDependencyTypes[RQT <: Tuple] <: Tuple = RQT match
    case EmptyTuple => EmptyTuple
    case Restricted[_, d] *: tail => d *: ExtractDependencyTypes[tail]

  type Collate[A, D <: Tuple] <: Tuple = A match
    case Restricted[_, d] =>
      Tuple.Concat[d, D]
    case _ =>
      D

  type CollateAll[A <: Tuple, D <: Tuple] <: Tuple = A match
    case EmptyTuple => D
    case h *: t => CollateAll[t, Collate[h, D]]

  def tupleExecute[T <: Tuple](t: T): Tuple =
    t match
      case EmptyTuple => EmptyTuple
      case (h: Restricted[_, _]) *: tail =>
        h.execute() *:
          tupleExecute(tail)

  trait Restricted[A, D <: Tuple] extends Dynamic:
    // Use inline with transparent return type for precise type inference
    transparent inline def selectDynamic(inline name: String): Any =
      ${ selectDynamicQuote[A, D]('this, 'name) }

    transparent inline def applyDynamic(inline name: String)(inline args: Any*): Any =
      ${ applyDynamicQuote[A, D]('this, 'name, 'args) }

    def execute(): A

  object Restricted:
    case class LinearRef[A, D <: Tuple](val fn: () => A) extends Restricted[A, D]:
      def execute(): A = fn()

  // Inline quote implementation for selectDynamic - builds Select AST directly
  private def selectDynamicQuote[A: Type, D <: Tuple: Type](
    self: Expr[Restricted[A, D]],
    name: Expr[String]
  )(using Quotes): Expr[Any] =
    import quotes.reflect.*

    val fieldName = name.valueOrAbort

    // Build the Select AST
    val linearRefExpr = '{ $self.asInstanceOf[Restricted.LinearRef[A, D]] }
    val fnCall = '{ $linearRefExpr.fn() }
    val selectTree = Select.unique(fnCall.asTerm, fieldName)

    // Extract the field type from the Select tree
    selectTree.tpe.asType match
      case '[fieldType] =>
        val resultExpr = selectTree.asExprOf[fieldType]
        '{
          Restricted.LinearRef[fieldType, D](() => $resultExpr): Restricted[fieldType, D]
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
          if restrictedType.typeSymbol.fullName == "linearfn.RestrictedDynamicQuotes$.Restricted" =>
          currentDeps = TypeRepr.of[Tuple.Concat].appliedTo(List(dArg, currentDeps))
        case _ => ()
      }

    // Build AST
    val linearRefExpr = '{ $self.asInstanceOf[Restricted.LinearRef[A, D]] }
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
              Restricted.LinearRef[rt, newD & Tuple](() => $resultExpr): Restricted[rt, newD & Tuple]
            }.asExprOf[Restricted[rt, newD & Tuple]]
        }
    }

  object LinearFn:
    def apply[AT <: Tuple, RQT <: Tuple]
    (args: AT)
    (fns: ToLinearRef[AT] => RQT)
    (using @implicitNotFound("Number of actual arguments must match the number of elements returned by fns")
    ev0: Tuple.Size[AT] =:= Tuple.Size[RQT])
    (using @implicitNotFound("Cannot extract dependencies, is the query affine?")
    ev2: ExtractDependencyTypes[RQT] <:< InverseMapDeps[RQT])
    (using @implicitNotFound("Failed to match restricted types")
    ev3: RQT =:= ToRestricted[ExtractResultTypes[RQT], ExtractDependencyTypes[RQT]])
    (using @implicitNotFound("Recursive definitions must be linear")
    ev4: ExpectedResult[AT] <:< ActualResult[RQT]) =
      val argsRefs = args.toArray.map(a => Restricted.LinearRef(() => a))
      val refsTuple = Tuple.fromArray(argsRefs).asInstanceOf[ToLinearRef[AT]]
      val exec = fns(refsTuple)
      println(exec)
      tupleExecute(exec)
end RestrictedDynamicQuotes
