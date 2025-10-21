package linearfn

import scala.quoted.*

object RestrictedMacros:
  import RestrictedDynamicMacros.Restricted
  def selectDynamicImpl[A: Type, D <: Tuple: Type](
    self: Expr[Restricted[A, D]],
    name: Expr[String]
  )(using Quotes): Expr[Any] =
    import quotes.reflect.*

    // Extract the field name at compile time
    val fieldName = name.valueOrAbort

    // Get the type representation of A
    val aType = TypeRepr.of[A]

    // Check if the field exists (look in ALL members, including inherited)
    val allMembers = aType.typeSymbol.memberFields ++ aType.typeSymbol.memberMethods
    val fieldMethod = allMembers.find(_.name == fieldName)

    fieldMethod match
      case None =>
        report.errorAndAbort(s"Field '$fieldName' not found on type ${aType.show}")

      case Some(method) =>
        // Get the return type of the field (handle both methods and fields)
        val returnType = method.tree match
          case DefDef(_, _, returnTpt, _) => returnTpt.tpe
          case ValDef(_, returnTpt, _) => returnTpt.tpe
          case _ => report.errorAndAbort(s"Cannot determine return type for $fieldName (unexpected tree: ${method.tree})")

        returnType.asType match
          case '[fieldType] =>
            // Generate code that creates a new Restricted wrapping the field access
            '{
              val linearRef = $self.asInstanceOf[Restricted.LinearRef[A, D]]
              Restricted.LinearRef[fieldType, D](() => {
                val result = linearRef.fn()
                // Use reflection to call the accessor method
                val method = result.getClass.getMethod($name)
                method.invoke(result).asInstanceOf[fieldType]
              }): Restricted[fieldType, D]  // Upcast to trait
            }.asExprOf[Restricted[fieldType, D]]

  def applyDynamicImpl[A: Type, D <: Tuple: Type](
    self: Expr[Restricted[A, D]],
    name: Expr[String],
    args: Expr[Seq[Any]]
  )(using Quotes): Expr[Any] =
    import quotes.reflect.*

    // Extract the method name at compile time
    val methodName = name.valueOrAbort

    // Get the type representation of A
    val aType = TypeRepr.of[A]

    // Find methods with matching name (look in ALL members, including inherited)
    val allMembers = aType.typeSymbol.memberFields ++ aType.typeSymbol.memberMethods
    val methods = allMembers.filter(_.name == methodName)

    if methods.isEmpty then
      report.errorAndAbort(s"Method '$methodName' not found on type ${aType.show}")

    // For now, take the first matching method
    // TODO: could add overload resolution based on argument types
    val method = methods.head

    // Get the return type (handle both methods and fields)
    val returnType = method.tree match
      case DefDef(_, _, returnTpt, _) => returnTpt.tpe
      case ValDef(_, returnTpt, _) => returnTpt.tpe
      case _ => report.errorAndAbort(s"Cannot determine return type for $methodName")

    // Extract argument types from the Seq expression to compute collated dependencies
    // For varargs, we get Varargs(...) which we can unwrap
    val argExprs = args.asTerm match {
      case Inlined(_, _, Typed(Repeated(argList, _), _)) => argList
      case Typed(Repeated(argList, _), _) => argList
      case _ => Nil
    }

    // Compute the collated dependency type by building it incrementally
    var currentDeps = TypeRepr.of[D]
    for argExpr <- argExprs do
      argExpr.tpe.dealias.widen match {
        case AppliedType(restrictedType, List(tArg, dArg))
          if restrictedType.typeSymbol.fullName == "linearfn.RestrictedDynamicMacros$.Restricted" =>
          // Build Tuple.Concat[dArg, currentDeps]
          val concatType = TypeRepr.of[Tuple.Concat].appliedTo(List(dArg, currentDeps))
          currentDeps = concatType
        case other =>
          // Non-Restricted argument, dependencies unchanged
          ()
      }

    // Simplify and dealias the computed dependency type to ensure it's properly evaluated
    val simplifiedDeps = currentDeps.dealias.simplified

    simplifiedDeps.asType match {
      case '[newD] =>
        returnType.asType match
          case '[rt] =>
            '{
              Restricted.LinearRef[rt, newD & Tuple](() => {
                val linearRef = $self.asInstanceOf[Restricted.LinearRef[A, D]]
                val result = linearRef.fn()
                val argSeq = $args

                // Execute any Restricted arguments to get their actual values
                val executedArgs = argSeq.map {
                  case r: Restricted[_, _] => r.execute()
                  case other => other
                }

                // Handle primitive type conversion for reflection
                val argClasses = executedArgs.map { arg =>
                  arg match {
                    case _: java.lang.Integer => classOf[Int]
                    case _: java.lang.Long => classOf[Long]
                    case _: java.lang.Double => classOf[Double]
                    case _: java.lang.Float => classOf[Float]
                    case _: java.lang.Boolean => classOf[Boolean]
                    case _: java.lang.Byte => classOf[Byte]
                    case _: java.lang.Short => classOf[Short]
                    case _: java.lang.Character => classOf[Char]
                    case other => other.getClass.asInstanceOf[Class[?]]
                  }
                }.toArray

                val method = result.getClass.getDeclaredMethod($name, argClasses*)
                method.invoke(result, executedArgs.map(_.asInstanceOf[Object])*).asInstanceOf[rt]
              }): Restricted[rt, newD & Tuple]
            }.asExprOf[Restricted[rt, newD & Tuple]]
    }
end RestrictedMacros
