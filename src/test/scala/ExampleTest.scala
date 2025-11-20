// COMMENTED OUT
// package test
// 
// import munit.FunSuite
// import linearfn.{HorizontalConstraint, RestrictedSelectable, VerticalConstraint, ops}
// import test.TestUtils
// 
// class ExampleTest extends FunSuite:
// 
//   import IntTypeOps.*
// 
//   test("Linear fn alone") {
//     val a = IntType(10)
//     val b = IntType(20)
// 
//     val result = RestrictedSelectable.RestrictedFn.customApply(
//       (vertical = VerticalConstraint.Affine, horizontal = HorizontalConstraint.ForAllRelevantForEachAffine)
//     )(Tuple1(a))(refs =>
//       Tuple1(refs._1)
//     )
//     assert(result._1.value == 10)
//   }
// 
//   test("Curry custom linear fn: f x y = (x + x, y)") {
//     // f :: Int -> Int %1 -> (Int, Int)
//     // f x y = (x + x, y)
//     // This should SUCCEED because:
//     // - x is non-linear (captured in closure, used twice)
//     // - y is linear (used exactly once in the linear function body)
// 
//     // The curried function: outer function takes x (non-linear), returns a linear function
//     val f: Int => (Tuple1[IntType] => Tuple1[IntType]) =
//       (x: Int) =>
//         // Return a linear function that takes y
//         RestrictedSelectable.RestrictedFn.customLinearFn(
//           (vertical = VerticalConstraint.Affine, horizontal = HorizontalConstraint.ForAllRelevantForEachAffine)
//         )(refs =>
//           val y = refs._1  // y is linear (Restricted[IntType])
//           Tuple1(IntType(x + x).add(y))  // x used twice (OK, captured), y used once (OK)
//         )
// 
//     // Apply it: f(10) returns a linear function that we can call with Tuple1(IntType(20))
//     val linearFn = f(10)
//     val result = linearFn(Tuple1(IntType(20)))
// 
//     assertEquals(result._1.value, 40)  // (x + x) + y = 20 + 20 = 40
//   }
// 
// 
//   test("Curry custom linear fn: using y twice in comprehension should FAIL") {
//     // Demonstrate that using a linear variable twice in a for-comprehension fails
//     val obtained = compileErrors("""
//       import IntTypeOps.*
// 
//       val f: Int => (Tuple1[IntType] => Tuple1[IntType]) =
//         (x: Int) =>
//           RestrictedSelectable.RestrictedFn.customLinearFn(
//             (vertical = VerticalConstraint.Affine, horizontal = HorizontalConstraint.ForAllRelevantForEachAffine)
//           )(refs =>
//             val y = refs._1
//             // Try to use y twice by adding it to itself
//             Tuple1(y.add(y))  // y used twice - should fail!
//           )
// 
//       val linearFn = f(10)
//       linearFn(Tuple1(IntType(20)))
//     """)
// 
//     assert(obtained.contains("Affine constraint"), s"Expected linearity error, got: $obtained")
//   }
// 
//   test("Curry linear fn: f x y = (x + x, y)") {
//     val f: Int => (Tuple1[IntType] => Tuple1[IntType]) =
//       (x: Int) =>
//         RestrictedSelectable.RestrictedFn.strictLinearFn(refs =>
//           val y = refs._1
//           Tuple1(IntType(x + x).add(y))
//         )
// 
//     val linearFn = f(10)
//     val result = linearFn(Tuple1(IntType(20)))
// 
//     assertEquals(result._1.value, 40)
//   }
// 
// 
//   test("Curry linear fn: using y twice in comprehension should FAIL") {
//     // Demonstrate that using a linear variable twice in a for-comprehension fails
//     val obtained = compileErrors(
//       """
//       import IntTypeOps.*
// 
//       val f: Int => (Tuple1[IntType] => Tuple1[IntType]) =
//         (x: Int) =>
//           RestrictedSelectable.RestrictedFn.strictLinearFn(refs =>
//             val y = refs._1
//             Tuple1(y.add(y))  // y used twice - should fail!
//           )
// 
//       val linearFn = f(10)
//       linearFn(Tuple1(IntType(20)))
//     """)
// 
//     assert(obtained.contains("Affine constraint"), s"Expected linearity error, got: $obtained")
//   }
// 
