package test.casestudies

/**
 * Implementation of Query to Datalog IR translation.
 * Separated from the main Query AST definitions for better code organization.
 */
object LinearDatalogImpl:

  /**
   * Translate a Query to Datalog IR.
   * Returns (Program, predicate name for this query, next fresh variable index)
   */
  def translate(
    query: Query[?],
    targetName: String,
    env: scala.collection.immutable.Map[Query[?], String],
    nextIdx: Int
  ): (IR.Program, String, Int) =
    query match
      case q: Query.EDB[?] => translateEDB(q, targetName, env, nextIdx)
      case q: Query.Filter[?] => translateFilter(q, targetName, env, nextIdx)
      case q: Query.Map[?, ?] => translateMap(q, targetName, env, nextIdx)
      case q: Query.FlatMap[?, ?] => translateFlatMap(q, targetName, env, nextIdx)
      case q: Query.Union[?] => translateUnion(q, targetName, env, nextIdx)
      case q: Query.IntensionalRef[?] => translateIntensionalRef(q, targetName, env, nextIdx)
      case q: Query.IntensionalPredicates[?] => translateIntensionalPredicates(q, targetName, env, nextIdx)
      case _ =>
        // Default case for unknown query types
        (IR.Program(scala.collection.immutable.ListMap.empty), targetName, nextIdx)

  private def translateEDB(
    query: Query.EDB[?],
    targetName: String,
    env: scala.collection.immutable.Map[Query[?], String],
    nextIdx: Int
  ): (IR.Program, String, Int) =
    // EDB generates a rule: targetName(v0, ..., vn) :- predicate(v0, ..., vn)
    val vars = (0 until query.arity).map(i => IR.Var(s"v${nextIdx + i}")).toList
    val bodyAtom = IR.Atom(query.predicate, vars)
    val headAtom = IR.Atom(targetName, vars)
    val rule = IR.Rule(headAtom, List(bodyAtom))

    val predicates = scala.collection.immutable.ListMap(targetName -> IR.PredicateDef(targetName, List(rule)))
    (IR.Program(predicates), targetName, nextIdx + query.arity)

  private def translateFilter(
    query: Query.Filter[?],
    targetName: String,
    env: scala.collection.immutable.Map[Query[?], String],
    nextIdx: Int
  ): (IR.Program, String, Int) =
    // Process the source query
    val (fromProg, fromPred, nextIdx1) = translate(query.$from, Query.freshPredName(), env, nextIdx)

    // Create variables for the input (assuming arity 2)
    var varIdx = nextIdx1
    val inputVars = (0 until 2).map { i =>
      val v = IR.Var(s"v$varIdx")
      varIdx += 1
      v
    }.toList

    // Extract constraints from the filter predicate
    val param = query.$pred.$param
    val filterBody = query.$pred.$body
    val constraints = exprToConstraints(filterBody.asInstanceOf[Expr[Boolean]], param, inputVars)

    // Create the filter rule
    val bodyAtom = IR.Atom(fromPred, inputVars)
    val headAtom = IR.Atom(targetName, inputVars)
    val rule = IR.Rule(headAtom, List(bodyAtom), constraints)

    val allPreds = fromProg.predicates + (targetName -> IR.PredicateDef(targetName, List(rule)))
    (IR.Program(allPreds), targetName, varIdx)

  private def translateMap(
    query: Query.Map[?, ?],
    targetName: String,
    env: scala.collection.immutable.Map[Query[?], String],
    nextIdx: Int
  ): (IR.Program, String, Int) =
    // Process the source query
    val (fromProg, fromPred, nextIdx1) = translate(query.$from, Query.freshPredName(), env, nextIdx)

    // Create variables for the input (assuming arity 2)
    val inputVars = List(IR.Var(s"v$nextIdx1"), IR.Var(s"v${nextIdx1 + 1}"))
    var varIdx = nextIdx1 + 2

    // Extract output terms from the map function
    val param = query.$query.$param
    val mapBody = query.$query.$body

    val headTerms = mapBody match
      case proj: Expr.Project[?] =>
        // Handle named tuple projection
        proj.$a.asInstanceOf[Product].productIterator.map {
          case s: Expr.Select[?] =>
            // Map field name to variable index
            val fieldName = s.$name
            val fieldIdx = fieldName.drop(1).toIntOption.getOrElse(0) - 1
            if fieldIdx >= 0 && fieldIdx < inputVars.length then
              inputVars(fieldIdx)
            else
              IR.Var(s"v$varIdx")
          case lit: Expr.ExprLit =>
            // Handle literal constants
            IR.Const(lit.i.toString)
          case _ =>
            IR.Var(s"v$varIdx")
        }.toList
      case _ =>
        // For non-tuple results, just use a variable
        List(IR.Var(s"v$varIdx"))

    // Create the map rule
    val bodyAtom = IR.Atom(fromPred, inputVars)
    val headAtom = IR.Atom(targetName, headTerms)
    val rule = IR.Rule(headAtom, List(bodyAtom))

    val allPreds = fromProg.predicates + (targetName -> IR.PredicateDef(targetName, List(rule)))
    (IR.Program(allPreds), targetName, varIdx)

  private def translateFlatMap(
    query: Query.FlatMap[?, ?],
    targetName: String,
    env: scala.collection.immutable.Map[Query[?], String],
    nextIdx: Int
  ): (IR.Program, String, Int) =
    // Process the source query
    val (fromProg, fromPred, nextIdx1) = translate(query.$from, Query.freshPredName(), env, nextIdx)

    // Create variables for the outer query (assuming arity 2 for now)
    val outerParam = query.$query.$param
    val outerVars = List(IR.Var(s"v$nextIdx1"), IR.Var(s"v${nextIdx1 + 1}"))
    var varIdx = nextIdx1 + 2

    // Analyze the inner query structure
    val innerQuery = query.$query.$body

    // Helper to resolve expressions to terms, with knowledge of both outer and inner refs
    def resolveExpr(expr: Expr[?], innerParam: Expr.Ref[?], innerVars: List[IR.Var], mapParam: Option[Expr.Ref[?]] = None): IR.Term = expr match
      case s: Expr.Select[?] =>
        val selectRef = s.$x
        // Use referential equality (eq) instead of == to avoid Expr.==
        if selectRef eq outerParam then
          // Selection on outer query result
          val fieldName = s.$name
          val fieldIdx = fieldName.drop(1).toIntOption.getOrElse(1) - 1
          if fieldIdx >= 0 && fieldIdx < outerVars.length then outerVars(fieldIdx)
          else IR.Var(s"v$varIdx")
        else if (selectRef eq innerParam) || mapParam.exists(_ eq selectRef) then
          // Selection on inner query result (could be from filter param or map param)
          val fieldName = s.$name
          val fieldIdx = fieldName.drop(1).toIntOption.getOrElse(1) - 1
          if fieldIdx >= 0 && fieldIdx < innerVars.length then innerVars(fieldIdx)
          else IR.Var(s"v$varIdx")
        else
          IR.Var(s"v$varIdx")
      case lit: Expr.ExprLit =>
        IR.Const(lit.i.toString)
      case _ =>
        IR.Var(s"v$varIdx")

    // Helper to extract constraints from boolean expressions
    def extractConstraints(expr: Expr[Boolean], innerParam: Expr.Ref[?], innerVars: List[IR.Var], mapParamOpt: Option[Expr.Ref[?]] = None): List[IR.Constraint] = expr match
      case Expr.Eq(left, right) =>
        List(IR.Eq(resolveExpr(left, innerParam, innerVars, mapParamOpt), resolveExpr(right, innerParam, innerVars, mapParamOpt)))
      case Expr.And(left, right) =>
        extractConstraints(left.asInstanceOf[Expr[Boolean]], innerParam, innerVars, mapParamOpt) ++
        extractConstraints(right.asInstanceOf[Expr[Boolean]], innerParam, innerVars, mapParamOpt)
      case _ =>
        List.empty

    // Analyze inner query to extract join pattern
    innerQuery match
      case mapQuery @ Query.Map(from, mapFun) =>
        // Inner query is a map, possibly over a filter
        from match
          case filterQuery @ Query.Filter(source, filterFun) =>
            // We have: flatMap(outer, o => filter(inner, i => pred).map(i => ...))
            // Generate join with constraints
            val (innerProg, innerPred, nextIdx2) = translate(source, Query.freshPredName(), env, varIdx)
            val innerParam = filterFun.$param
            val innerVars = List(IR.Var(s"v$nextIdx2"), IR.Var(s"v${nextIdx2 + 1}"))
            varIdx = nextIdx2 + 2

            // Now handle the map to get its param
            val mapParam = mapFun.$param.asInstanceOf[Expr.Ref[Any]]

            val constraints = extractConstraints(filterFun.$body.asInstanceOf[Expr[Boolean]], innerParam, innerVars, Some(mapParam))

            val mapBody = mapFun.$body

            // Extract output terms from map
            val outputTerms = mapBody match
              case proj: Expr.Project[?] =>
                // Convert NamedTuple to List[Expr[?]]
                val tuple = proj.$a
                val elems = tuple.asInstanceOf[Product].productIterator.map {
                  case s: Expr.Select[?] => resolveExpr(s, innerParam, innerVars, Some(mapParam))
                  case lit: Expr.ExprLit => IR.Const(lit.i.toString)
                  case other => resolveExpr(other.asInstanceOf[Expr[?]], innerParam, innerVars, Some(mapParam))
                }.toList
                elems
              case _ => List(IR.Var(s"v$varIdx"))

            // Unify variables in constraints where possible
            // Build substitution map from constraints like Var == Var
            var substitution = scala.collection.mutable.Map[IR.Var, IR.Var]()
            val remainingConstraints = constraints.filter {
              case IR.Eq(v1: IR.Var, v2: IR.Var) =>
                // Prefer to keep the earlier variable (lower index)
                val var1Idx = v1.name.drop(1).toInt
                val var2Idx = v2.name.drop(1).toInt
                if var1Idx < var2Idx then
                  substitution(v2) = v1
                else
                  substitution(v1) = v2
                false // Remove this constraint
              case _ => true // Keep constraints involving constants
            }

            // Apply substitution to all terms
            def substitute(term: IR.Term): IR.Term = term match
              case v: IR.Var => substitution.getOrElse(v, v)
              case c => c

            val substOuterVars = outerVars.map(substitute)
            val substInnerVars = innerVars.map(substitute)
            val substOutputTerms = outputTerms.map(substitute)

            val fromAtom = IR.Atom(fromPred, substOuterVars)
            val innerAtom = IR.Atom(innerPred, substInnerVars)
            val headAtom = IR.Atom(targetName, substOutputTerms)
            val rule = IR.Rule(headAtom, List(fromAtom, innerAtom), remainingConstraints)

            val allPreds = fromProg.predicates ++ innerProg.predicates +
              (targetName -> IR.PredicateDef(targetName, List(rule)))
            (IR.Program(allPreds), targetName, varIdx)

          case other =>
            // Just a map over a plain query (no filter, so no constraints to unify)
            val (innerProg, innerPred, nextIdx2) = translate(other, Query.freshPredName(), env, varIdx)
            val innerParam = mapFun.$param.asInstanceOf[Expr.Ref[Any]]
            val innerVars = List(IR.Var(s"v$nextIdx2"), IR.Var(s"v${nextIdx2 + 1}"))
            varIdx = nextIdx2 + 2

            val mapBody = mapFun.$body
            val outputTerms = mapBody match
              case proj: Expr.Project[?] =>
                val tuple = proj.$a
                val elems = tuple.asInstanceOf[Product].productIterator.map {
                  case s: Expr.Select[?] => resolveExpr(s, innerParam, innerVars, Some(innerParam))
                  case lit: Expr.ExprLit => IR.Const(lit.i.toString)
                  case other => resolveExpr(other.asInstanceOf[Expr[?]], innerParam, innerVars, Some(innerParam))
                }.toList
                elems
              case _ => List(IR.Var(s"v$varIdx"))

            val fromAtom = IR.Atom(fromPred, outerVars)
            val innerAtom = IR.Atom(innerPred, innerVars)
            val headAtom = IR.Atom(targetName, outputTerms)
            val rule = IR.Rule(headAtom, List(fromAtom, innerAtom))

            val allPreds = fromProg.predicates ++ innerProg.predicates +
              (targetName -> IR.PredicateDef(targetName, List(rule)))
            (IR.Program(allPreds), targetName, varIdx)

      case _ =>
        // Fallback to original simple implementation
        val (innerProg, innerPred, nextIdx2) = translate(innerQuery, Query.freshPredName(), env, varIdx)
        val fromAtom = IR.Atom(fromPred, outerVars)
        val innerAtom = IR.Atom(innerPred, List(IR.Var(s"v$nextIdx2")))
        val headAtom = IR.Atom(targetName, List(IR.Var(s"v$nextIdx2")))
        val rule = IR.Rule(headAtom, List(fromAtom, innerAtom))

        val allPreds = fromProg.predicates ++ innerProg.predicates +
          (targetName -> IR.PredicateDef(targetName, List(rule)))
        (IR.Program(allPreds), targetName, nextIdx2 + 1)

  private def translateUnion(
    query: Query.Union[?],
    targetName: String,
    env: scala.collection.immutable.Map[Query[?], String],
    nextIdx: Int
  ): (IR.Program, String, Int) =
    // Process both sides
    val (leftProg, leftPred, nextIdx1) = translate(query.$left, Query.freshPredName(), env, nextIdx)
    val (rightProg, rightPred, nextIdx2) = translate(query.$right, Query.freshPredName(), env, nextIdx1)

    // Create two rules: one for each union branch
    // Both rules have the same head with the same variables (assuming arity 2)
    val vars = List(IR.Var(s"v$nextIdx2"), IR.Var(s"v${nextIdx2 + 1}"))

    val leftBodyAtom = IR.Atom(leftPred, vars)
    val rightBodyAtom = IR.Atom(rightPred, vars)

    val leftHeadAtom = IR.Atom(targetName, vars)
    val rightHeadAtom = IR.Atom(targetName, vars)

    val leftRule = IR.Rule(leftHeadAtom, List(leftBodyAtom))
    val rightRule = IR.Rule(rightHeadAtom, List(rightBodyAtom))

    val combinedPreds = leftProg.predicates ++ rightProg.predicates +
      (targetName -> IR.PredicateDef(targetName, List(leftRule, rightRule)))
    (IR.Program(combinedPreds), targetName, nextIdx2 + 2)

  private def translateIntensionalRef(
    query: Query.IntensionalRef[?],
    targetName: String,
    env: scala.collection.immutable.Map[Query[?], String],
    nextIdx: Int
  ): (IR.Program, String, Int) =
    env.get(query) match
      case Some(predName) =>
        // Reference to an already-defined intensional predicate (assuming arity 2)
        val vars = List(IR.Var(s"v$nextIdx"), IR.Var(s"v${nextIdx + 1}"))
        val bodyAtom = IR.Atom(predName, vars)
        val headAtom = IR.Atom(targetName, vars)
        val rule = IR.Rule(headAtom, List(bodyAtom))

        val predicates = scala.collection.immutable.ListMap(targetName -> IR.PredicateDef(targetName, List(rule)))
        (IR.Program(predicates), targetName, nextIdx + 2)
      case None =>
        // Unresolved reference
        (IR.Program(scala.collection.immutable.ListMap.empty), targetName, nextIdx)

  private def translateIntensionalPredicates(
    query: Query.IntensionalPredicates[?],
    targetName: String,
    env: scala.collection.immutable.Map[Query[?], String],
    nextIdx: Int
  ): (IR.Program, String, Int) =
    // Process all predicates in the fixed point
    // Each IntensionalRef gets assigned a predicate name like "idb0", "idb1", etc.
    var allPredicates = scala.collection.immutable.ListMap.empty[String, IR.PredicateDef]
    var nextIdx1 = nextIdx

    // Build environment mapping IntensionalRef -> predicate name
    val newEnv = query.predicates.keys.map { ref =>
      (ref.asInstanceOf[Query[?]], s"idb${ref.id}")
    }.toMap

    // Translate each recursive predicate definition
    query.predicates.foreach { (ref, queryDef) =>
      val predName = s"idb${ref.id}"
      val (prog, _, nextIdx2) = translate(queryDef, predName, env ++ newEnv, nextIdx1)
      allPredicates = allPredicates ++ prog.predicates
      nextIdx1 = nextIdx2
    }

    // Return the predicate at the specified index
    val sortedRefs = query.predicates.keys.toList.sortBy(_.id)
    val resultPredName = s"idb${sortedRefs(query.idx).id}"
    (IR.Program(allPredicates), resultPredName, nextIdx1)

  /**
   * Convert an expression to a list of terms for use in rule heads.
   * Used by Filter to extract constraint terms.
   */
  private def exprToTerm(expr: Expr[?], param: Expr.Ref[?], inputVars: List[IR.Var]): IR.Term = expr match
    case s: Expr.Select[?] =>
      val fieldName = s.$name
      val fieldIdx = fieldName.drop(1).toIntOption.getOrElse(0) - 1
      if fieldIdx >= 0 && fieldIdx < inputVars.size then
        inputVars(fieldIdx)
      else
        IR.Var(s"v${inputVars.length + fieldIdx}")
    case lit: Expr.ExprLit =>
      IR.Const(lit.i.toString)
    case _ =>
      IR.Var(s"v${inputVars.length}")

  /**
   * Convert a boolean expression to a list of constraints.
   * Handles equality and conjunction.
   */
  private def exprToConstraints(expr: Expr[Boolean], param: Expr.Ref[?], inputVars: List[IR.Var]): List[IR.Constraint] = expr match
    case Expr.Eq(left, right) =>
      List(IR.Eq(exprToTerm(left, param, inputVars), exprToTerm(right, param, inputVars)))
    case Expr.And(left, right) =>
      exprToConstraints(left.asInstanceOf[Expr[Boolean]], param, inputVars) ++
      exprToConstraints(right.asInstanceOf[Expr[Boolean]], param, inputVars)
    case _ =>
      List.empty
