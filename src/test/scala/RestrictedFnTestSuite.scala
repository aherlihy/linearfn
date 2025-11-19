 package test

 import munit.FunSuite
 import linearfn.{RestrictedSelectable, Multiplicity, ops, consumed, unconsumed, repeatable, RestrictedFnBase}


 /**
  * Shared test suite for all RestrictedFn implementations.
  * Takes an instance of RestrictedFnBase and runs tests on it using path-dependent types.
  */
 abstract class RestrictedFnTestSuite(impl: RestrictedFnBase, name: String) extends FunSuite:

   test(s"$name: single argument - return unchanged") {
     val str = "hello"
     val result = impl.RestrictedFn.apply(Multiplicity.Affine)(Tuple1(str))(refs =>
       Tuple1(refs._1)
     )
     assertEquals(result, Tuple1(str))
   }

   test(s"$name: two arguments - return unchanged") {
     val str = "hello"
     val num = 42
     val result = impl.RestrictedFn.apply(Multiplicity.Affine)((str, num))(refs =>
       (refs._1, refs._2)
     )
     assertEquals(result, (str, num))
   }

   test(s"$name: two arguments - switch order") {
     val str = "hello"
     val num = 42
     val result = impl.RestrictedFn.apply(Multiplicity.Affine)((str, num))(refs =>
       (refs._2, refs._1)
     )
     assertEquals(result, (num, str))
   }
