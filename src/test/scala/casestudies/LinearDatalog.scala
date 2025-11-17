package test.casestudies

import linearfn.{ErrorMsg, HorizontalConstraint, RestrictedSelectable, VerticalConstraint, consumed, ops, repeatable, restricted, restrictedReturn, unrestricted}

import scala.annotation.implicitNotFound

/**
 * Case Study: Linear Datalog
 */

// Column-level AST
trait Expr[Result] extends Selectable:
  type Fields = NamedTuple.Map[NamedTuple.From[Result], Expr]
  def selectDynamic(fieldName: String) = Expr.Select(this, fieldName)
  def ==(y: Expr[?]): Expr[Boolean] = Expr.Eq(this, y)
  
object Expr:
  type StripExpr[E] = E match
    case Expr[b] => b

  case class Select[A]($x: Expr[A], $name: String) extends Expr[A]
  case class Project[A <: NamedTuple.AnyNamedTuple]($a: A) extends Expr[NamedTuple.Map[A, StripExpr]]
  type IsTupleOfExpr[A <: NamedTuple.AnyNamedTuple] = Tuple.Union[NamedTuple.DropNames[A]] <:< Expr[?]

  given [A <: NamedTuple.AnyNamedTuple : IsTupleOfExpr]: Conversion[A, Project[A]] with
    def apply(x: A): Project[A] = Project(x)

  private var refCount = 0
  case class Ref[A]() extends Expr[A]:
    private val $id = refCount
    refCount += 1
    override def toString: String = s"Ref(id = ${$id})"

  case class Fun[A, B]($param: Ref[A], $body: B)

  case class Plus($x: Expr[Int], $y: Expr[Int]) extends Expr[Int]
  case class Eq($x: Expr[?], $y: Expr[?]) extends Expr[Boolean]
  case class And($x: Expr[Boolean], $y: Expr[Boolean]) extends Expr[Boolean]
  extension(x: Expr[Int])
    def +(y: Expr[Int]): Expr[Int] = Plus(x, y)
  extension(x: Expr[Boolean])  
    def &&(y: Expr[Boolean]): Expr[Boolean] = And(x, y)

  case class ExprLit(i: Int) extends Expr[Int]

  // Helper to convert Expr to IR.Term
  private var varCount = 0
  def exprToTerms(expr: Expr[?], paramName: String = "X"): (List[IR.Term], Map[Expr[?], IR.Var]) =
    var bindings = Map.empty[Expr[?], IR.Var]
    def freshVar(hint: String = "V"): IR.Var =
      val v = IR.Var(s"$hint$varCount")
      varCount += 1
      v

    def go(e: Expr[?]): IR.Term = e match
      case r: Ref[?] =>
        bindings.getOrElse(r, {
          val v = IR.Var(paramName)
          bindings = bindings + (r -> v)
          v
        })
      case Select(x, name) =>
        // For field selection, create a variable based on the field name
        bindings.getOrElse(e, {
          val v = IR.Var(name.capitalize)
          bindings = bindings + (e -> v)
          v
        })
      case ExprLit(i) =>
        IR.Const(i.toString)
      case _ =>
        bindings.getOrElse(e, {
          val v = freshVar()
          bindings = bindings + (e -> v)
          v
        })

    (List(go(expr)), bindings)

// Query-level AST
@ops
class Query[A]():
  @repeatable
  def flatMap[B](@restrictedReturn f: Expr.Ref[A] => Query[B]): Query[B] =
    val ref = Expr.Ref[A]()
    Query.FlatMap(this, Expr.Fun(ref, f(ref)))

  @repeatable
  def map[B](@unrestricted f: Expr.Ref[A] => Expr[B]): Query[B] =
    val ref = Expr.Ref[A]()
    Query.Map(this, Expr.Fun(ref, f(ref)))

  @repeatable
  def withFilter(@unrestricted predicate: Expr.Ref[A] => Expr[Boolean]): Query[A] =
    val ref = Expr.Ref[A]()
    Query.Filter[A](this, Expr.Fun(ref, predicate(ref)))

  @repeatable
  def filter(@unrestricted predicate: Expr.Ref[A] => Expr[Boolean]): Query[A] =
    withFilter(predicate)

  @repeatable
  def union(that: Query[A]): Query[A] =
    Query.Union[A](this, that)

  @repeatable
  def unionAll(@restricted that: Query[A]): Query[A] =
    Query.Union[A](this, that)

  // Convert Query to IR and generate Datalog string
  def peek(): String =
    val resultPred = Query.freshPredName()
    val (program, _, _) = this.toIR(resultPred, scala.collection.immutable.Map.empty, 0)
    program.toDatalog

  // Internal method to convert Query to IR
  // Returns (Program, predicate name for this query, next fresh predicate index)
  def toIR(targetName: String, env: scala.collection.immutable.Map[Query[?], String], nextIdx: Int): (IR.Program, String, Int) =
    this match
      case q: Query.EDB[?] => q.toIRImpl(targetName, env, nextIdx)
      case q: Query.Filter[?] => q.toIRImpl(targetName, env, nextIdx)
      case q: Query.Map[?, ?] => q.toIRImpl(targetName, env, nextIdx)
      case q: Query.FlatMap[?, ?] => q.toIRImpl(targetName, env, nextIdx)
      case q: Query.Union[?] => q.toIRImpl(targetName, env, nextIdx)
      case q: Query.IntensionalRef[?] => q.toIRImpl(targetName, env, nextIdx)
      case q: Query.IntensionalPredicates[?] => q.toIRImpl(targetName, env, nextIdx)
      case _ =>
        // Default case for unknown query types
        (IR.Program(scala.collection.immutable.Map.empty), targetName, nextIdx)

object Query:
  private var predCounter = 0
  private def freshPredName(): String =
    val name = s"p$predCounter"
    predCounter += 1
    name

  case class EDB[A](predicate: String, arity: Int = 2) extends Query[A]:
    override def toString: String = s"EDB(predicate = ${predicate})"

    def toIRImpl(targetName: String, env: scala.collection.immutable.Map[Query[?], String], nextIdx: Int): (IR.Program, String, Int) =
      // EDB generates a rule: targetName(v0, ..., vn) :- predicate(v0, ..., vn)
      val vars = (0 until arity).map(i => IR.Var(s"v${nextIdx + i}")).toList
      val bodyAtom = IR.Atom(predicate, vars)
      val headAtom = IR.Atom(targetName, vars)
      val rule = IR.Rule(headAtom, List(bodyAtom))

      val predicates = scala.collection.immutable.Map(targetName -> IR.PredicateDef(targetName, List(rule)))
      (IR.Program(predicates), targetName, nextIdx + arity)

  // Factory method that returns Query[A] instead of EDB[A]
  def edb[A](predicate: String): Query[A] = EDB[A](predicate)

  case class Filter[A]($from: Query[A], $pred: Expr.Fun[A, Expr[Boolean]]) extends Query[A]:
    override def toString: String = s"Filter(from = ${$from}, pred = ${$pred})"

    def toIRImpl(targetName: String, env: scala.collection.immutable.Map[Query[?], String], nextIdx: Int): (IR.Program, String, Int) =
      // Process the source query
      val (fromProg, fromPred, nextIdx1) = $from.toIR(freshPredName(), env, nextIdx)

      // Convert the predicate to constraints
      var varIdx = nextIdx1
      val inputVars = (0 until 2).map { i => // Assuming arity 2 for now
        val v = IR.Var(s"v$varIdx")
        varIdx += 1
        v
      }.toList

      // Helper to convert Expr to Term with variable mapping
      def exprToTerm(expr: Expr[?]): IR.Term = expr match
        case s: Expr.Select[?] =>
          // Map field selection to input variable
          val fieldName = s.$name
          val fieldIdx = fieldName.drop(1).toIntOption.getOrElse(0) - 1
          if fieldIdx >= 0 && fieldIdx < inputVars.size then
            inputVars(fieldIdx)
          else
            IR.Var(s"v$varIdx")
        case lit: Expr.ExprLit =>
          IR.Const(lit.i.toString)
        case _ =>
          IR.Var(s"v$varIdx")

      // Convert boolean expression to constraints
      def exprToConstraints(expr: Expr[Boolean]): List[IR.Constraint] = expr match
        case Expr.Eq(left, right) =>
          List(IR.Eq(exprToTerm(left), exprToTerm(right)))
        case Expr.And(left, right) =>
          exprToConstraints(left.asInstanceOf[Expr[Boolean]]) ++
          exprToConstraints(right.asInstanceOf[Expr[Boolean]])
        case _ =>
          List.empty

      val constraints = exprToConstraints($pred.$body)

      val bodyAtom = IR.Atom(fromPred, inputVars)
      val headAtom = IR.Atom(targetName, inputVars)
      val rule = IR.Rule(headAtom, List(bodyAtom), constraints)

      val combinedPreds = fromProg.predicates + (targetName -> IR.PredicateDef(targetName, List(rule)))
      (IR.Program(combinedPreds), targetName, varIdx)

  case class Map[A, B]($from: Query[A], $query: Expr.Fun[A, Expr[B]]) extends Query[B]:
    override def toString: String = s"Map(from = ${$from}, query = ${$query})"

    def toIRImpl(targetName: String, env: scala.collection.immutable.Map[Query[?], String], nextIdx: Int): (IR.Program, String, Int) =
      // Process the source query
      val (fromProg, fromPred, nextIdx1) = $from.toIR(freshPredName(), env, nextIdx)

      // Convert the result expression to terms
      val param = $query.$param
      val body = $query.$body

      body match
        case proj: Expr.Project[?] =>
          // Extract terms from the tuple
          val tuple = proj.$a.asInstanceOf[Tuple]

          // Build a mapping from the parameter's fields to variables
          // We need to figure out the arity of the input predicate
          // For now, assume it matches the structure of the parameter
          var varIdx = nextIdx1
          val fieldToVar = scala.collection.mutable.Map[String, IR.Var]()

          // Create variables for input fields - we'll use v0, v1, v2, etc.
          // For a NamedTuple type like (i1: Int, i2: Int), we need 2 variables
          val inputVars = (0 until tuple.size).map { i =>
            val v = IR.Var(s"v$varIdx")
            varIdx += 1
            v
          }.toList

          // Map field selections to the corresponding input variables
          // This is a simplified approach - in reality we'd need type information
          val headTerms = tuple.productIterator.toList.zipWithIndex.map {
            case (s: Expr.Select[?], _) =>
              // For field selections, we need to find which input field this refers to
              // For simplicity, assume fields are named i1, i2, etc. and map to v0, v1, etc.
              val fieldName = s.$name
              // Extract the field index from the name (e.g., "i1" -> 0, "i2" -> 1)
              val fieldIdx = fieldName.drop(1).toIntOption.getOrElse(0) - 1
              if fieldIdx >= 0 && fieldIdx < inputVars.size then
                inputVars(fieldIdx)
              else
                IR.Var(s"v$varIdx")
            case (lit: Expr.ExprLit, _) =>
              // Handle literal constants
              IR.Const(lit.i.toString)
            case (other, idx) =>
              IR.Var(s"v$varIdx")
          }

          val bodyAtom = IR.Atom(fromPred, inputVars)
          val headAtom = IR.Atom(targetName, headTerms)
          val rule = IR.Rule(headAtom, List(bodyAtom))

          val combinedPreds = fromProg.predicates + (targetName -> IR.PredicateDef(targetName, List(rule)))
          (IR.Program(combinedPreds), targetName, varIdx)
        case expr =>
          // Simple expression, just create a variable for it
          val (terms, _) = Expr.exprToTerms(expr, "v0")
          val bodyAtom = IR.Atom(fromPred, List(IR.Var("v0")))
          val headAtom = IR.Atom(targetName, terms)
          val rule = IR.Rule(headAtom, List(bodyAtom))

          val combinedPreds = fromProg.predicates + (targetName -> IR.PredicateDef(targetName, List(rule)))
          (IR.Program(combinedPreds), targetName, nextIdx1 + 1)

  case class FlatMap[A, B]($from: Query[A], $query: Expr.Fun[A, Query[B]]) extends Query[B]:
    override def toString: String = s"FlatMap(from = ${$from}, query = ${$query})"

    def toIRImpl(targetName: String, env: scala.collection.immutable.Map[Query[?], String], nextIdx: Int): (IR.Program, String, Int) =
      // Process the source query
      val (fromProg, fromPred, nextIdx1) = $from.toIR(freshPredName(), env, nextIdx)

      // Create variables for the outer query (assuming arity 2 for now)
      val outerParam = $query.$param
      val outerVars = List(IR.Var(s"v$nextIdx1"), IR.Var(s"v${nextIdx1 + 1}"))
      var varIdx = nextIdx1 + 2

      // Now we need to inline the inner query processing with knowledge of outer variables
      // We'll analyze the inner query structure
      val innerQuery = $query.$body

      // Helper to resolve expressions to terms, with knowledge of both outer and inner refs
      // innerParam can be either the filter param or the map param - they refer to the same query result
      def resolveExpr(expr: Expr[?], innerParam: Expr.Ref[?], innerVars: List[IR.Var], mapParam: Option[Expr.Ref[?]] = None): IR.Term = expr match
        case s: Expr.Select[?] =>
          // Check if this is a selection on the outer param or inner param
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
        case mapQuery @ Map(from, mapFun) =>
          // Inner query is a map, possibly over a filter
          from match
            case filterQuery @ Filter(source, filterFun) =>
              // We have: flatMap(outer, o => filter(inner, i => pred).map(i => ...))
              // Generate join with constraints
              val (innerProg, innerPred, nextIdx2) = source.toIR(freshPredName(), env, varIdx)
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
              val (innerProg, innerPred, nextIdx2) = other.toIR(freshPredName(), env, varIdx)
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
          val (innerProg, innerPred, nextIdx2) = innerQuery.toIR(freshPredName(), env, varIdx)
          val fromAtom = IR.Atom(fromPred, outerVars)
          val innerAtom = IR.Atom(innerPred, List(IR.Var(s"v$nextIdx2")))
          val headAtom = IR.Atom(targetName, List(IR.Var(s"v$nextIdx2")))
          val rule = IR.Rule(headAtom, List(fromAtom, innerAtom))

          val allPreds = fromProg.predicates ++ innerProg.predicates +
            (targetName -> IR.PredicateDef(targetName, List(rule)))
          (IR.Program(allPreds), targetName, nextIdx2 + 1)

  case class Union[A]($left: Query[A], $right: Query[A]) extends Query[A]:
    override def toString: String = s"Union(left = ${$left}, right = ${$right})"

    def toIRImpl(targetName: String, env: scala.collection.immutable.Map[Query[?], String], nextIdx: Int): (IR.Program, String, Int) =
      // Process both sides
      val (leftProg, leftPred, nextIdx1) = $left.toIR(freshPredName(), env, nextIdx)
      val (rightProg, rightPred, nextIdx2) = $right.toIR(freshPredName(), env, nextIdx1)

      // Create two rules: one for each union branch
      val v = IR.Var(s"v$nextIdx2")
      val leftRule = IR.Rule(IR.Atom(targetName, List(v)), List(IR.Atom(leftPred, List(v))))
      val rightRule = IR.Rule(IR.Atom(targetName, List(v)), List(IR.Atom(rightPred, List(v))))

      val allPreds = leftProg.predicates ++ rightProg.predicates +
        (targetName -> IR.PredicateDef(targetName, List(leftRule, rightRule)))
      (IR.Program(allPreds), targetName, nextIdx2 + 1)

  var intensionalRefCount = 0
  case class IntensionalRef[A]() extends Query[A]:
    private val idx = intensionalRefCount
    intensionalRefCount += 1
    override def toString: String = s"IntensionalRef(id = ${idx})"

    def toIRImpl(targetName: String, env: scala.collection.immutable.Map[Query[?], String], nextIdx: Int): (IR.Program, String, Int) =
      // IntensionalRef refers to a recursive predicate
      val predName = env.getOrElse(this, s"idb$idx")
      (IR.Program(scala.collection.immutable.Map.empty), predName, nextIdx)

  case class IntensionalPredicates[R](predicates: scala.collection.immutable.Map[IntensionalRef[Any], Query[Any]], idx: Int) extends Query[R]:
    override def toString: String = s"IntensionalPredicates(predicates = ${predicates}, idx = ${idx})"

    def toIRImpl(targetName: String, env: scala.collection.immutable.Map[Query[?], String], nextIdx: Int): (IR.Program, String, Int) =
      // Process all the predicates in the fixed point
      var allPredicates = scala.collection.immutable.Map.empty[String, IR.PredicateDef]
      var nextIdx1 = nextIdx
      val newEnv = predicates.keys.zipWithIndex.map { (ref, i) =>
        (ref.asInstanceOf[Query[?]], s"idb$i")
      }.toMap

      predicates.foreach { (ref, query) =>
        val predName = newEnv(ref)
        val (prog, _, nextIdx2) = query.toIR(predName, env ++ newEnv, nextIdx1)
        allPredicates = allPredicates ++ prog.predicates
        nextIdx1 = nextIdx2
      }

      // Return the predicate at the specified index
      val resultPred = newEnv.toList.sortBy(_._2).apply(idx)._2
      (IR.Program(allPredicates), resultPred, nextIdx1)
  export RestrictedSelectable.RestrictedFn.strictApply as fix

  /**
   * customFix: Educational version showing how the library works.
   *
   * Base linearity constraints are imported from the library via the builder type.
   * The library's LinearFnBuilder type can only be constructed when base constraints
   * are satisfied, so requesting it via `using` enforces those constraints.
   *
   * We add ONLY 2 additional constraints here:
   * - Tuple.Size[QT] =:= Tuple.Size[RQT] (strictness)
   * - Tuple.Union[QT] <:< Query[?] (domain-specific)
   */
  def fixedPoint[QT <: Tuple, RQT <: Tuple](
    bases: QT
  )(fns: RestrictedSelectable.RestrictedFn.LinearFn[QT, RQT])(
    using
      builder: RestrictedSelectable.RestrictedFn.LinearFnBuilder[
        VerticalConstraint.Affine.type,
        HorizontalConstraint.ForAllRelevantForEachAffine.type,
        QT, RQT
      ],
      @implicitNotFound("customFix requires same number of args and returns")
      evStrict: Tuple.Size[QT] =:= Tuple.Size[RQT],
      @implicitNotFound("customFix requires all arguments to be Query types")
      evQuery: Tuple.Union[QT] <:< Query[?]
  ) = {
    val argsRefs = (0 until bases.size).map(_ => IntensionalRef[Any]())
    val restrictedRefs = argsRefs.map(a => RestrictedSelectable.makeRestrictedRef(() => a)).toArray
    val refsTuple = Tuple.fromArray(restrictedRefs).asInstanceOf[RestrictedSelectable.ToRestrictedRef[QT]]
    val exec = fns(refsTuple)
    val evaluatedT = RestrictedSelectable.tupleExecute(exec)
    val unioned = bases.toArray.zip(evaluatedT.toArray).map { (baseQ, evalQ) =>
      baseQ.asInstanceOf[Query[Any]].union(evalQ.asInstanceOf[Query[Any]])
    }
    val predicates = argsRefs.zip(unioned).toMap
    (0 to argsRefs.size).map(i => IntensionalPredicates(predicates, i))
  }

// Intermediate Representation
object IR:
  case class Program(predicates: Map[String, PredicateDef]):
    def toDatalog: String =
      predicates.values.map(_.toDatalog).mkString("\n\n")

  case class PredicateDef(name: String, rules: List[Rule]):
    def toDatalog: String =
      rules.map(_.toDatalog(name)).mkString("\n")

  case class Rule(head: Atom, body: List[Atom], constraints: List[Constraint] = List.empty):
    def toDatalog(predicateName: String): String =
      val headStr = s"$predicateName(${head.terms.map(_.toDatalog).mkString(", ")})"
      if body.isEmpty && constraints.isEmpty then
        s"$headStr."
      else
        val bodyParts = body.map(atom => s"${atom.predicate}(${atom.terms.map(_.toDatalog).mkString(", ")})")
        val constraintParts = constraints.map(_.toDatalog)
        val allParts = bodyParts ++ constraintParts
        val bodyStr = allParts.mkString(", ")
        s"$headStr :- $bodyStr."

  case class Atom(predicate: String, terms: List[Term])

  sealed trait Constraint:
    def toDatalog: String

  case class Eq(left: Term, right: Term) extends Constraint:
    def toDatalog: String = s"${left.toDatalog} == ${right.toDatalog}"

  sealed trait Term:
    def toDatalog: String

  case class Var(name: String) extends Term:
    def toDatalog: String = name

  case class Const(value: String) extends Term:
    def toDatalog: String = value