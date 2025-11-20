// COMMENTED OUT
// package test
// 
// import munit.FunSuite
// import scala.annotation.experimental
// import linearfn.RestrictedSelectable
// 
// /**
//  * Comprehensive tests for @unrestricted parameters and implicit conversion.
//  *
//  * Verifies:
//  * 1. Plain values (implicitly converted to EmptyTuple) don't affect dependencies
//  * 2. @unrestricted parameters don't contribute their dependencies to result type
//  * 3. @unrestricted parameters can be reused without linearity violations
//  */
// @experimental
// class UnrestrictedParameterTest extends FunSuite:
//   import OpsExampleOps.*
// 
//   // ===== Test 1: Plain values with implicit conversion =====
// 
//   test("plain value implicitly converted - EmptyTuple doesn't affect result type") {
//     val ex = OpsExample("base")
// 
//     val result = RestrictedSelectable.RestrictedFn.apply(Tuple1(ex))(refs =>
//       // "plain config" is implicitly converted to Restricted[String, EmptyTuple]
//       // Result type should be Tuple.Concat[EmptyTuple, D] = D
//       val updated = refs._1.allUnrestrictedArgs("prefix-", "-suffix")
//       Tuple1(updated)
//     )
// 
//     assertEquals(result._1.name, "prefix-base-suffix")
//   }
// 
//   test("mixing plain value and Restricted value - only Restricted contributes dependencies") {
//     val ex1 = OpsExample("first")
//     val ex2 = OpsExample("second")
// 
//     val result = RestrictedSelectable.RestrictedFn.apply((ex1, ex2))(refs =>
//       // refs._2 has dependencies, "config" is plain (EmptyTuple)
//       // Result should include D2 from tracked param, not from config
//       val combined = refs._1.restrictedProductArg_UnrestrictedPrimitiveArg(refs._2, "plain config")
//       (combined, combined)
//     )
// 
//     assert(result._1.name.contains("first + second"))
//   }
// 
//   // ===== Test 2: @unrestricted with Restricted values that have dependencies =====
// 
//   test("@unrestricted parameter with Restricted value - dependencies not included in result") {
//     val ex1 = OpsExample("first")
//     val ex2 = OpsExample("second")
//     val ex3 = OpsExample("third")
// 
//     // This should compile because config's D2 is not in the result type
//     val result = RestrictedSelectable.RestrictedFn.apply((ex1, ex2, ex3))(refs =>
//       // Pass refs._2 to @unrestricted param, but also return it separately
//       // This works because @unrestricted params don't contribute dependencies
//       val combined = refs._1.restrictedProductArg_UnrestrictedPrimitiveArg(refs._3, refs._2.value)
//       (combined, refs._2, refs._2)  // refs._2 can be returned multiple times because it was @unrestricted!
//     )
// 
//     assert(result._1.name.contains("first + third"), s"Expected to contain 'first + third', got: ${result._1.value}")
//     assertEquals(result._2.name, "second")
//   }
// 
//   test("@unrestricted parameter allows returning the argument separately") {
//     val ex1 = OpsExample("first")
//     val configEx = OpsExample("config-value")
// 
//     // This demonstrates that @unrestricted params don't affect linearity
//     val result = RestrictedSelectable.RestrictedFn.apply((ex1, configEx))(refs =>
//       // Use refs._2 as @unrestricted param, but also return it
//       val updated = refs._1.allUnrestrictedArgs(refs._2.name, "-suffix")
//       (updated, refs._2)  // Can return refs._2 because it wasn't tracked!
//     )
// 
//     assert(result._1.name.contains("config-value"), s"Expected 'config-value' in ${result._1.name}")
//     assertEquals(result._2.name, "config-value")
//   }
// 
//   // ===== Test 3: @unrestricted allows multiple uses (no linearity violation) =====
// 
//   test("@unrestricted with Restricted value having dependencies can be used with result") {
//     val ex1 = OpsExample("base")
//     val ex2 = OpsExample("config")
// 
//     val result = RestrictedSelectable.RestrictedFn.apply((ex1, ex2))(refs =>
//       // refs._2 used as @unrestricted param and also returned
//       val v1 = refs._1.allUnrestrictedArgs(refs._2.name, "-suffix1")
//       (v1, refs._2)  // Can return refs._2 because it was @unrestricted!
//     )
// 
//     assert(result._1.name.contains("config"), s"Expected 'config' in ${result._1.name}")
//     assertEquals(result._2.name, "config")
//   }
// 
//   // ===== Test 4: Complex scenarios with multiple @unrestricted =====
// 
//   test("multiple @unrestricted params - can all be returned separately") {
//     val ex = OpsExample("base")
//     val config1 = "config1"
//     val config2 = "config2"
// 
//     val result = RestrictedSelectable.RestrictedFn.apply((ex, config1, config2))(refs =>
//       // Both config params are @unrestricted
//       val v1 = refs._1.allUnrestrictedArgs(refs._2, refs._3)
//       // Can return both config values because they were @unrestricted
//       (v1, refs._2, refs._3)
//     )
// 
//     assert(result._1.name == "config1baseconfig2", s"Expected 'config1baseconfig2', got: ${result._1.name}")
//     assertEquals(result._2, "config1")
//     assertEquals(result._3, "config2")
//   }
// 
//   test("complexOp: mixed tracked and @unrestricted - only tracked affect result type") {
//     val ex1 = OpsExample("first")
//     val ex2 = OpsExample("second")
//     val ex3 = OpsExample("third")
//     val config = "config"
// 
//     // complexOp(first: D1, config: D2 @unrestricted, second: D3)
//     // Result should be Tuple.Concat[D1, Tuple.Concat[D3, D]]
//     val result = RestrictedSelectable.RestrictedFn.apply((ex1, ex2, ex3, config))(refs =>
//       val r = refs._1.mixedTrackedAndUnrestricted(refs._2, refs._4, refs._3)
//       // We can return config multiple times because it's @unrestricted!
//       (r, refs._4, refs._4, refs._4)
//     )
// 
//     assert(result._1.name.contains("first + second + third"), s"Expected to contain 'first + second + third', got: ${result._1.name}")
//     assertEquals(result._2, "config")
//   }
// 
//   // ===== Test 5: Compile-time errors that should NOT happen =====
// 
//   test("@unrestricted prevents duplicate use error that would occur with tracked param") {
//     val ex = OpsExample("base")
//     val config = "config"
// 
//     // If config were tracked, using refs._2 multiple times would fail
//     // But since it's @unrestricted, this works fine
//     val result = RestrictedSelectable.RestrictedFn.apply((ex, config))(refs =>
//       val v1 = refs._1.allUnrestrictedArgs(refs._2, refs._2)  // Same param twice!
//       (v1, refs._2)  // Can also return it!
//     )
// 
//     assertEquals(result._1.name, "configbaseconfig")
//     assertEquals(result._2, "config")
//   }
