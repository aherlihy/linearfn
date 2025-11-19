 package test

 import linearfn.{RestrictedSelectable, Multiplicity}


 /**
  * Tests for RestrictedSelectable implementation.
  */
 class RestrictedSelectableTest extends RestrictedFnTestSuite(RestrictedSelectable, "RestrictedSelectable"):
   test("RestrictedSelectable: manual non-linear") {
     val obtained = compileErrors("""
       val str = "hello"
       val num = 42
       val actual = 5
       val result = RestrictedSelectable.RestrictedFn.apply(Multiplicity.Linear)((str, num))(refs =>
         val tmp: RestrictedSelectable.Restricted[Int, (0, 0), EmptyTuple] = RestrictedSelectable.Restricted.RestrictedRef[Int, (0, 0), EmptyTuple](() => actual)
         (tmp, refs._1)
       )
     """)
     assert(obtained.contains(TestUtils.multiplicityConstraintFailed), s"obtained: $obtained")
   }
   test("RestrictedSelectable: manual non-affine") {
     val obtained = compileErrors("""
       val str = "hello"
       val num = 42
       val actual = 5
       val result = RestrictedSelectable.RestrictedFn.apply(Multiplicity.Linear)((str, num))(refs =>
         val tmp: RestrictedSelectable.Restricted[Int, Tuple1[0], EmptyTuple] = RestrictedSelectable.Restricted.RestrictedRef[Int, Tuple1[0], EmptyTuple](() => actual)
         (tmp, refs._1)
       )
     """)
     assert(obtained.contains(TestUtils.multiplicityConstraintFailed), s"obtained: $obtained")
   }

   // Note: RestrictedSelectable doesn't support primitive operators since it uses Selectable, not Dynamic
   // test("RestrictedSelectable: dependencies are tracked for primitive operator") {
   //   val obtained = compileErrors("""
   //     val str = "hello"
   //     val num = 42
   //     RestrictedSelectable.RestrictedFn.apply(Multiplicity.Linear)((str, num))(refs => (refs._2 + refs._2, refs._1))
   //   """)
   //   assert(obtained.contains(TestUtils.horizontalAffineFailed), s"obtained: $obtained")
   // }

   // Runtime tests for field and method access
   test("valid field access on case class") {
     case class Person(name: String, age: Int)
     val person = Person("Alice", 30)
     // Field access works - just verify no runtime errors
     RestrictedSelectable.RestrictedFn.apply(Multiplicity.Affine)(Tuple1(person))(refs =>
       val _age = refs._1.age  // This should compile and run without errors
       Tuple1(refs._1)
     )
     assert(true) // If we get here, field access worked
   }

   test("field access fails for non-existing field") {
     val obtained = compileErrors("""
       case class Person(name: String, age: Int)
       val person = Person("Alice", 30)
       RestrictedSelectable.RestrictedFn.apply(Multiplicity.Linear)(Tuple1(person))(refs =>
         val x = refs._1.nonExistentField
         Tuple1(refs._1)
       )
     """)
     assert(obtained.contains("value nonExistentField is not a member"), s"obtained: $obtained")
   }
//
//   // Note: RestrictedSelectable doesn't support field access on primitive types (uses Selectable, not Dynamic)
//   // test("valid field access on primitive type") {
//   //   val str = "hello"
//   //   val result = RestrictedSelectable.RestrictedFn.apply(Multiplicity.Linear)(Tuple1(str))(refs =>
//   //     val len = refs._1.length
//   //     Tuple1(len)
//   //   )
//   //   assertEquals(result, Tuple1(5))
//   // }
 src/test/scala/RestrictedSelectableTest.scala