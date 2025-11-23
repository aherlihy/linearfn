package test

import linearfn.{Multiplicity,ops, restricted, restrictedReturn, unrestricted}
import linearfn.RestrictedSelectable.{given, *}
import munit.FunSuite

@ops
class ProductConnectiveFns(val s: String):
  def higherOrderCombine(@restrictedReturn f: String => ProductConnectiveFns): ProductConnectiveFns =
    combine(f("hof"))
  def combine(other: ProductConnectiveFns): ProductConnectiveFns =
    new ProductConnectiveFns(s"${this.s} + ${other.s}")


type ForAllLinearConnective[RT <: Tuple] = ComposedConnective[RT, Multiplicity.Unrestricted, Multiplicity.Linear]
object ForAllLinearConnective:
  def apply[RT <: Tuple]
    (values: RT): ForAllLinearConnective[RT] =
    ComposedConnective[RT, Multiplicity.Unrestricted, Multiplicity.Linear](values)

type ForEachLinearConnective[RT <: Tuple] = ComposedConnective[RT, Multiplicity.Linear, Multiplicity.Unrestricted]
object ForEachLinearConnective:
  def apply[RT <: Tuple]
  (values: RT): ForEachLinearConnective[RT] =
    ComposedConnective[RT, Multiplicity.Linear, Multiplicity.Unrestricted](values)

type ForAllAffineConnective[RT <: Tuple] = ComposedConnective[RT, Multiplicity.Unrestricted, Multiplicity.Affine]
object ForAllAffineConnective:
  def apply[RT <: Tuple]
  (values: RT): ForAllAffineConnective[RT] =
    ComposedConnective[RT, Multiplicity.Unrestricted, Multiplicity.Affine](values)

type ForEachAffineConnective[RT <: Tuple] = ComposedConnective[RT, Multiplicity.Affine, Multiplicity.Unrestricted]
object ForEachAffineConnective:
  def apply[RT <: Tuple]
  (values: RT): ForEachAffineConnective[RT] =
    ComposedConnective[RT, Multiplicity.Affine, Multiplicity.Unrestricted](values)

type ForAllRelevantConnective[RT <: Tuple] = ComposedConnective[RT, Multiplicity.Unrestricted, Multiplicity.Relevant]
object ForAllRelevantConnective:
  def apply[RT <: Tuple]
  (values: RT): ForAllRelevantConnective[RT] =
    ComposedConnective[RT, Multiplicity.Unrestricted, Multiplicity.Relevant](values)

type ForEachRelevantConnective[RT <: Tuple] = ComposedConnective[RT, Multiplicity.Relevant, Multiplicity.Unrestricted]
object ForEachRelevantConnective:
  def apply[RT <: Tuple]
  (values: RT): ForEachRelevantConnective[RT] =
    ComposedConnective[RT, Multiplicity.Relevant, Multiplicity.Unrestricted](values)

object ProductConnectiveFns:
  def forAllLinearFn[QT <: Tuple, RT <: Tuple](
    bases: QT
  )(fns: RestrictedFn.LinearFn[QT, ForAllLinearConnective[RT]])(
    using builder: RestrictedFn.LinearFnBuilder[
      Multiplicity.Linear,
      QT,
      ForAllLinearConnective[RT]
    ]
  ): ExtractResultTypes[RT] =
    builder.execute(bases)(fns)

  def forEachLinearFn[QT <: Tuple, RT <: Tuple](
    bases: QT
  )(fns: RestrictedFn.LinearFn[QT, ForEachLinearConnective[RT]])(
    using builder: RestrictedFn.LinearFnBuilder[
      Multiplicity.Linear,
      QT,
      ForEachLinearConnective[RT]
    ]
  ): ExtractResultTypes[RT] =
    builder.execute(bases)(fns)

  def forAllAffineFn[QT <: Tuple, RT <: Tuple](
    bases: QT
  )(fns: RestrictedFn.LinearFn[QT, ForAllAffineConnective[RT]])(
    using builder: RestrictedFn.LinearFnBuilder[
      Multiplicity.Affine,
      QT,
      ForAllAffineConnective[RT]
    ]
  ): ExtractResultTypes[RT] =
    builder.execute(bases)(fns)

  def forEachAffineFn[QT <: Tuple, RT <: Tuple](
    bases: QT
  )(fns: RestrictedFn.LinearFn[QT, ForEachAffineConnective[RT]])(
    using builder: RestrictedFn.LinearFnBuilder[
      Multiplicity.Affine,
      QT,
      ForEachAffineConnective[RT]
    ]
  ): ExtractResultTypes[RT] =
    builder.execute(bases)(fns)

  def forAllRelevantFn[QT <: Tuple, RT <: Tuple](
    bases: QT
  )(fns: RestrictedFn.LinearFn[QT, ForAllRelevantConnective[RT]])(
    using builder: RestrictedFn.LinearFnBuilder[
      Multiplicity.Relevant,
      QT,
      ForAllRelevantConnective[RT]
    ]
  ): ExtractResultTypes[RT] =
    builder.execute(bases)(fns)

  def forEachRelevantFn[QT <: Tuple, RT <: Tuple](
    bases: QT
  )(fns: RestrictedFn.LinearFn[QT, ForEachRelevantConnective[RT]])(
    using builder: RestrictedFn.LinearFnBuilder[
      Multiplicity.Relevant,
      QT,
      ForEachRelevantConnective[RT]
    ]
  ): ExtractResultTypes[RT] =
    builder.execute(bases)(fns)

class CustomProductConnectiveTest extends FunSuite:
  import ProductConnectiveFnsOps.*
  test("Custom Product Connective with for-all-linear, one combined result") {
    val pcTest1 = new ProductConnectiveFns("a")
    val pcTest2 = new ProductConnectiveFns("b")
    val result = ProductConnectiveFns.forAllLinearFn((pcTest1, pcTest2))((p1, p2) =>
      ForAllLinearConnective(Tuple1(p1.combine(p2)))
    )
    assertEquals(result._1.s, "a + b")
  }
  test("Custom Product Connective with for-all-linear, one combined HOF result") {
    val pcTest1 = new ProductConnectiveFns("a")
    val pcTest2 = new ProductConnectiveFns("b")
    val result = ProductConnectiveFns.forAllLinearFn((pcTest1, pcTest2))((p1, p2) =>
      ForAllLinearConnective(Tuple1(p1.higherOrderCombine((i) => p2)))
    )
    assertEquals(result._1.s, "a + b")
  }
  test("Custom Product Connective with for-all-linear, two uncombined results") {
    val pcTest1 = new ProductConnectiveFns("a")
    val pcTest2 = new ProductConnectiveFns("b")
    val result = ProductConnectiveFns.forAllLinearFn((pcTest1, pcTest2))((p1, p2) =>
      ForAllLinearConnective((p1, p2))
    )
    assertEquals(result._1.s, "a")
    assertEquals(result._2.s, "b")
  }
  test("NEGATIVE: ForAll-Linear not relevant - missing argument") {
    val obtained = compileErrors("""
      import ProductConnectiveFnsOps.*
      val pcTest1 = new ProductConnectiveFns("a")
      val pcTest2 = new ProductConnectiveFns("b")
      ProductConnectiveFns.forAllLinearFn((pcTest1, pcTest2))((p1, p2) =>
        ForAllLinearConnective(Tuple1(p1))  // Error: p2 not used, violates relevant
      )
    """)
    assert(
      obtained.contains(TestUtils.noGivenInstance),
      s"Expected ForAll-Linear error but got: $obtained"
    )
  }

  test("NEGATIVE: ForAll-Linear not affine - argument used twice") {
    val obtained = compileErrors("""
      import ProductConnectiveFnsOps.*
      val pcTest1 = new ProductConnectiveFns("a")
      val pcTest2 = new ProductConnectiveFns("b")
      ProductConnectiveFns.forAllLinearFn((pcTest1, pcTest2))((p1, p2) =>
        ForAllLinearConnective(Tuple1(p1.combine(p2).combine(p1)))  // Error: p1 used twice, violates affine
      )
    """)
    assert(
      obtained.contains(TestUtils.noGivenInstance),
      s"Expected ForAll-Linear error but got: $obtained"
    )
  }

  test("NEGATIVE: ForAll-Linear not relevant and not affine") {
    val obtained = compileErrors("""
      import ProductConnectiveFnsOps.*
      val pcTest1 = new ProductConnectiveFns("a")
      val pcTest2 = new ProductConnectiveFns("b")
      ProductConnectiveFns.forAllLinearFn((pcTest1, pcTest2))((p1, p2) =>
        ForAllLinearConnective(Tuple1(p1.combine(p1)))  // Error: p1 used twice, p2 not used
      )
    """)
    assert(
      obtained.contains(TestUtils.noGivenInstance),
      s"Expected ForAll-Linear error but got: $obtained"
    )
  }

  test("ForAll-Linear with multiple returns - each argument used exactly once") {
    val pcTest1 = new ProductConnectiveFns("a")
    val pcTest2 = new ProductConnectiveFns("b")
    val pcTest3 = new ProductConnectiveFns("c")
    val result = ProductConnectiveFns.forAllLinearFn((pcTest1, pcTest2, pcTest3))((p1, p2, p3) =>
      ForAllLinearConnective((p1, p2, p3))  // OK: each arg used exactly once across all returns
    )
    assertEquals(result._1.s, "a")
    assertEquals(result._2.s, "b")
    assertEquals(result._3.s, "c")
  }

  test("NEGATIVE: ForAll-Linear with multiple returns - argument appears in two returns") {
    val obtained = compileErrors("""
      import ProductConnectiveFnsOps.*
      val pcTest1 = new ProductConnectiveFns("a")
      val pcTest2 = new ProductConnectiveFns("b")
      ProductConnectiveFns.forAllLinearFn((pcTest1, pcTest2))((p1, p2) =>
        ForAllLinearConnective((p1, p1))  // Error: p1 used twice (in both returns), p2 not used
      )
    """)
    assert(
      obtained.contains(TestUtils.noGivenInstance),
      s"Expected ForAll-Linear error but got: $obtained"
    )
  }
  test("Custom Product Connective with for-each-linear, one combined result") {
    val pcTest1 = new ProductConnectiveFns("a")
    val pcTest2 = new ProductConnectiveFns("b")
    val result = ProductConnectiveFns.forEachLinearFn((pcTest1, pcTest2))((p1, p2) =>
      ForEachLinearConnective(Tuple1(p1.combine(p2)))
    )
    assertEquals(result._1.s, "a + b")
  }
  test("NEGATIVE: ForEach-Linear not relevant - missing argument in return") {
    val obtained = compileErrors("""
      import ProductConnectiveFnsOps.*
      val pcTest1 = new ProductConnectiveFns("a")
      val pcTest2 = new ProductConnectiveFns("b")
      ProductConnectiveFns.forEachLinearFn((pcTest1, pcTest2))((p1, p2) =>
        ForEachLinearConnective(Tuple1(p1))  // Error: p2 not in return, violates relevant
      )
    """)
    assert(
      obtained.contains(TestUtils.noGivenInstance),
      s"Expected ForEach-Linear error but got: $obtained"
    )
  }

  test("NEGATIVE: ForEach-Linear not affine - argument used twice in same return") {
    val obtained = compileErrors("""
      import ProductConnectiveFnsOps.*
      val pcTest1 = new ProductConnectiveFns("a")
      val pcTest2 = new ProductConnectiveFns("b")
      ProductConnectiveFns.forEachLinearFn((pcTest1, pcTest2))((p1, p2) =>
        ForEachLinearConnective(Tuple1(p1.combine(p1)))  // Error: p1 used twice in return, violates affine
      )
    """)
    assert(
      obtained.contains(TestUtils.noGivenInstance),
      s"Expected ForEach-Linear error but got: $obtained"
    )
  }
  test("Custom Product Connective with for-each-linear, one combined HOF result") {
    val pcTest1 = new ProductConnectiveFns("a")
    val pcTest2 = new ProductConnectiveFns("b")
    val result = ProductConnectiveFns.forEachLinearFn((pcTest1, pcTest2))((p1, p2) =>
      ForEachLinearConnective(Tuple1(p1.higherOrderCombine(i => p2)))
    )
    assertEquals(result._1.s, "a + b")
  }
  test("Custom Product Connective with for-each-linear, two combined results") {
    val pcTest1 = new ProductConnectiveFns("a")
    val pcTest2 = new ProductConnectiveFns("b")
    val result = ProductConnectiveFns.forEachLinearFn((pcTest1, pcTest2))((p1, p2) =>
      ForEachLinearConnective((p1.combine(p2), p2.combine(p1)))
    )
    assertEquals(result._1.s, "a + b")
    assertEquals(result._2.s, "b + a")
  }
  test("Custom Product Connective with for-each-linear, two combined HOF results") {
    val pcTest1 = new ProductConnectiveFns("a")
    val pcTest2 = new ProductConnectiveFns("b")
    val result = ProductConnectiveFns.forEachLinearFn((pcTest1, pcTest2))((p1, p2) =>
      ForEachLinearConnective((p1.higherOrderCombine(i => p2), p2.higherOrderCombine(i => p1)))
    )
    assertEquals(result._1.s, "a + b")
    assertEquals(result._2.s, "b + a")
  }

  test("ForEach-Linear with three returns - each must use all arguments exactly once") {
    val pcTest1 = new ProductConnectiveFns("a")
    val pcTest2 = new ProductConnectiveFns("b")
    val pcTest3 = new ProductConnectiveFns("c")
    val result = ProductConnectiveFns.forEachLinearFn((pcTest1, pcTest2, pcTest3))((p1, p2, p3) =>
      ForEachLinearConnective((
        p1.combine(p2).combine(p3),  // All three args used exactly once
        p2.combine(p3).combine(p1),  // All three args used exactly once
        p3.combine(p1).combine(p2)   // All three args used exactly once
      ))
    )
    assertEquals(result._1.s, "a + b + c")
    assertEquals(result._2.s, "b + c + a")
    assertEquals(result._3.s, "c + a + b")
  }

  test("NEGATIVE: ForEach-Linear with multiple returns - one return violates linear") {
    val obtained = compileErrors("""
      import ProductConnectiveFnsOps.*
      val pcTest1 = new ProductConnectiveFns("a")
      val pcTest2 = new ProductConnectiveFns("b")
      val pcTest3 = new ProductConnectiveFns("c")
      ProductConnectiveFns.forEachLinearFn((pcTest1, pcTest2, pcTest3))((p1, p2, p3) =>
        ForEachLinearConnective((p1.combine(p2).combine(p3), p1.combine(p2)))  // Error: second return missing p3
      )
    """)
    assert(
      obtained.contains(TestUtils.noGivenInstance),
      s"Expected ForEach-Linear error but got: $obtained"
    )
  }

  // ForAllAffine tests
  test("Custom Product Connective with for-all-affine, one combined result") {
    val pcTest1 = new ProductConnectiveFns("a")
    val pcTest2 = new ProductConnectiveFns("b")
    val result = ProductConnectiveFns.forAllAffineFn((pcTest1, pcTest2))((p1, p2) =>
      ForAllAffineConnective(Tuple1(p1.combine(p2)))
    )
    assertEquals(result._1.s, "a + b")
  }

  test("Custom Product Connective with for-all-affine, two uncombined results") {
    val pcTest1 = new ProductConnectiveFns("a")
    val pcTest2 = new ProductConnectiveFns("b")
    val result = ProductConnectiveFns.forAllAffineFn((pcTest1, pcTest2))((p1, p2) =>
      ForAllAffineConnective((p1, p2))
    )
    assertEquals(result._1.s, "a")
    assertEquals(result._2.s, "b")
  }

  test("NEGATIVE: ForAll-Affine not affine - argument used twice") {
    val obtained = compileErrors("""
      import ProductConnectiveFnsOps.*
      val pcTest1 = new ProductConnectiveFns("a")
      val pcTest2 = new ProductConnectiveFns("b")
      ProductConnectiveFns.forAllAffineFn((pcTest1, pcTest2))((p1, p2) =>
        ForAllAffineConnective(Tuple1(p1.combine(p2).combine(p1)))  // Error: p1 used twice, violates affine
      )
    """)
    assert(
      obtained.contains(TestUtils.noGivenInstance),
      s"Expected ForAll-Affine error but got: $obtained"
    )
  }

  test("ForAll-Affine allows missing argument") {
    val pcTest1 = new ProductConnectiveFns("a")
    val pcTest2 = new ProductConnectiveFns("b")
    val result = ProductConnectiveFns.forAllAffineFn((pcTest1, pcTest2))((p1, p2) =>
      ForAllAffineConnective(Tuple1(p1))  // OK: p2 not used, affine doesn't require relevant
    )
    assertEquals(result._1.s, "a")
  }

  test("ForAll-Affine allows each argument used once across different returns") {
    val pcTest1 = new ProductConnectiveFns("a")
    val pcTest2 = new ProductConnectiveFns("b")
    val pcTest3 = new ProductConnectiveFns("c")
    val result = ProductConnectiveFns.forAllAffineFn((pcTest1, pcTest2, pcTest3))((p1, p2, p3) =>
      ForAllAffineConnective((p1, p2, p3))  // OK: each arg used exactly once across all returns
    )
    assertEquals(result._1.s, "a")
    assertEquals(result._2.s, "b")
    assertEquals(result._3.s, "c")
  }

  test("NEGATIVE: ForAll-Affine with multiple returns - argument used in two different returns") {
    val obtained = compileErrors("""
      import ProductConnectiveFnsOps.*
      val pcTest1 = new ProductConnectiveFns("a")
      val pcTest2 = new ProductConnectiveFns("b")
      ProductConnectiveFns.forAllAffineFn((pcTest1, pcTest2))((p1, p2) =>
        ForAllAffineConnective((p1, p1))  // Error: p1 used in both returns, violates affine
      )
    """)
    assert(
      obtained.contains(TestUtils.noGivenInstance),
      s"Expected ForAll-Affine error but got: $obtained"
    )
  }

  // ForEachAffine tests
  test("Custom Product Connective with for-each-affine, one combined result") {
    val pcTest1 = new ProductConnectiveFns("a")
    val pcTest2 = new ProductConnectiveFns("b")
    val result = ProductConnectiveFns.forEachAffineFn((pcTest1, pcTest2))((p1, p2) =>
      ForEachAffineConnective(Tuple1(p1.combine(p2)))
    )
    assertEquals(result._1.s, "a + b")
  }

  test("Custom Product Connective with for-each-affine, two combined results") {
    val pcTest1 = new ProductConnectiveFns("a")
    val pcTest2 = new ProductConnectiveFns("b")
    val result = ProductConnectiveFns.forEachAffineFn((pcTest1, pcTest2))((p1, p2) =>
      ForEachAffineConnective((p1.combine(p2), p2.combine(p1)))
    )
    assertEquals(result._1.s, "a + b")
    assertEquals(result._2.s, "b + a")
  }

  test("NEGATIVE: ForEach-Affine not affine - argument used twice in same return") {
    val obtained = compileErrors("""
      import ProductConnectiveFnsOps.*
      val pcTest1 = new ProductConnectiveFns("a")
      val pcTest2 = new ProductConnectiveFns("b")
      ProductConnectiveFns.forEachAffineFn((pcTest1, pcTest2))((p1, p2) =>
        ForEachAffineConnective(Tuple1(p1.combine(p1)))  // Error: p1 used twice in return, violates affine
      )
    """)
    assert(
      obtained.contains(TestUtils.noGivenInstance),
      s"Expected ForEach-Affine error but got: $obtained"
    )
  }

  test("ForEach-Affine allows missing argument in return") {
    val pcTest1 = new ProductConnectiveFns("a")
    val pcTest2 = new ProductConnectiveFns("b")
    val result = ProductConnectiveFns.forEachAffineFn((pcTest1, pcTest2))((p1, p2) =>
      ForEachAffineConnective(Tuple1(p1))  // OK: p2 not in return, affine doesn't require relevant
    )
    assertEquals(result._1.s, "a")
  }

  test("ForEach-Affine with three returns - each uses different subset") {
    val pcTest1 = new ProductConnectiveFns("a")
    val pcTest2 = new ProductConnectiveFns("b")
    val pcTest3 = new ProductConnectiveFns("c")
    val result = ProductConnectiveFns.forEachAffineFn((pcTest1, pcTest2, pcTest3))((p1, p2, p3) =>
      ForEachAffineConnective((p1.combine(p2), p2.combine(p3), p1.combine(p3)))  // OK: no duplicates within each return
    )
    assertEquals(result._1.s, "a + b")
    assertEquals(result._2.s, "b + c")
    assertEquals(result._3.s, "a + c")
  }

  test("NEGATIVE: ForEach-Affine with multiple returns - one return has duplicate") {
    val obtained = compileErrors("""
      import ProductConnectiveFnsOps.*
      val pcTest1 = new ProductConnectiveFns("a")
      val pcTest2 = new ProductConnectiveFns("b")
      ProductConnectiveFns.forEachAffineFn((pcTest1, pcTest2))((p1, p2) =>
        ForEachAffineConnective((p1.combine(p2), p1.combine(p1)))  // Error: second return uses p1 twice
      )
    """)
    assert(
      obtained.contains(TestUtils.noGivenInstance),
      s"Expected ForEach-Affine error but got: $obtained"
    )
  }

  // ForAllRelevant tests
  test("Custom Product Connective with for-all-relevant, one combined result") {
    val pcTest1 = new ProductConnectiveFns("a")
    val pcTest2 = new ProductConnectiveFns("b")
    val result = ProductConnectiveFns.forAllRelevantFn((pcTest1, pcTest2))((p1, p2) =>
      ForAllRelevantConnective(Tuple1(p1.combine(p2)))
    )
    assertEquals(result._1.s, "a + b")
  }

  test("Custom Product Connective with for-all-relevant, two uncombined results") {
    val pcTest1 = new ProductConnectiveFns("a")
    val pcTest2 = new ProductConnectiveFns("b")
    val result = ProductConnectiveFns.forAllRelevantFn((pcTest1, pcTest2))((p1, p2) =>
      ForAllRelevantConnective((p1, p2))
    )
    assertEquals(result._1.s, "a")
    assertEquals(result._2.s, "b")
  }

  test("NEGATIVE: ForAll-Relevant not relevant - missing argument") {
    val obtained = compileErrors("""
      import ProductConnectiveFnsOps.*
      val pcTest1 = new ProductConnectiveFns("a")
      val pcTest2 = new ProductConnectiveFns("b")
      ProductConnectiveFns.forAllRelevantFn((pcTest1, pcTest2))((p1, p2) =>
        ForAllRelevantConnective(Tuple1(p1))  // Error: p2 not used, violates relevant
      )
    """)
    assert(
      obtained.contains(TestUtils.noGivenInstance),
      s"Expected ForAll-Relevant error but got: $obtained"
    )
  }

  test("ForAll-Relevant allows argument used twice") {
    val pcTest1 = new ProductConnectiveFns("a")
    val pcTest2 = new ProductConnectiveFns("b")
    val result = ProductConnectiveFns.forAllRelevantFn((pcTest1, pcTest2))((p1, p2) =>
      ForAllRelevantConnective(Tuple1(p1.combine(p2).combine(p1)))  // OK: p1 used twice, relevant doesn't require affine
    )
    assertEquals(result._1.s, "a + b + a")
  }

  test("ForAll-Relevant with multiple returns - allows argument reuse across returns") {
    val pcTest1 = new ProductConnectiveFns("a")
    val pcTest2 = new ProductConnectiveFns("b")
    val pcTest3 = new ProductConnectiveFns("c")
    val result = ProductConnectiveFns.forAllRelevantFn((pcTest1, pcTest2, pcTest3))((p1, p2, p3) =>
      ForAllRelevantConnective((p1.combine(p2), p2.combine(p3), p1.combine(p3)))  // OK: all args used, reuse allowed
    )
    assertEquals(result._1.s, "a + b")
    assertEquals(result._2.s, "b + c")
    assertEquals(result._3.s, "a + c")
  }

  test("NEGATIVE: ForAll-Relevant with multiple returns - one argument never used") {
    val obtained = compileErrors("""
      import ProductConnectiveFnsOps.*
      val pcTest1 = new ProductConnectiveFns("a")
      val pcTest2 = new ProductConnectiveFns("b")
      val pcTest3 = new ProductConnectiveFns("c")
      ProductConnectiveFns.forAllRelevantFn((pcTest1, pcTest2, pcTest3))((p1, p2, p3) =>
        ForAllRelevantConnective((p1, p1))  // Error: p2 and p3 never used
      )
    """)
    assert(
      obtained.contains(TestUtils.noGivenInstance),
      s"Expected ForAll-Relevant error but got: $obtained"
    )
  }

  // ForEachRelevant tests
  test("Custom Product Connective with for-each-relevant, one combined result") {
    val pcTest1 = new ProductConnectiveFns("a")
    val pcTest2 = new ProductConnectiveFns("b")
    val result = ProductConnectiveFns.forEachRelevantFn((pcTest1, pcTest2))((p1, p2) =>
      ForEachRelevantConnective(Tuple1(p1.combine(p2)))
    )
    assertEquals(result._1.s, "a + b")
  }

  test("Custom Product Connective with for-each-relevant, two combined results") {
    val pcTest1 = new ProductConnectiveFns("a")
    val pcTest2 = new ProductConnectiveFns("b")
    val result = ProductConnectiveFns.forEachRelevantFn((pcTest1, pcTest2))((p1, p2) =>
      ForEachRelevantConnective((p1.combine(p2), p2.combine(p1)))
    )
    assertEquals(result._1.s, "a + b")
    assertEquals(result._2.s, "b + a")
  }

  test("NEGATIVE: ForEach-Relevant not relevant - missing argument in return") {
    val obtained = compileErrors("""
      import ProductConnectiveFnsOps.*
      val pcTest1 = new ProductConnectiveFns("a")
      val pcTest2 = new ProductConnectiveFns("b")
      ProductConnectiveFns.forEachRelevantFn((pcTest1, pcTest2))((p1, p2) =>
        ForEachRelevantConnective(Tuple1(p1))  // Error: p2 not in return, violates relevant
      )
    """)
    assert(
      obtained.contains(TestUtils.noGivenInstance),
      s"Expected ForEach-Relevant error but got: $obtained"
    )
  }

  test("ForEach-Relevant allows argument used twice in same return") {
    val pcTest1 = new ProductConnectiveFns("a")
    val pcTest2 = new ProductConnectiveFns("b")
    val result = ProductConnectiveFns.forEachRelevantFn((pcTest1, pcTest2))((p1, p2) =>
      ForEachRelevantConnective(Tuple1(p1.combine(p2).combine(p1)))  // OK: p1 used twice, relevant doesn't require affine
    )
    assertEquals(result._1.s, "a + b + a")
  }

  test("ForEach-Relevant with three returns - each must use all arguments") {
    val pcTest1 = new ProductConnectiveFns("a")
    val pcTest2 = new ProductConnectiveFns("b")
    val pcTest3 = new ProductConnectiveFns("c")
    val result = ProductConnectiveFns.forEachRelevantFn((pcTest1, pcTest2, pcTest3))((p1, p2, p3) =>
      ForEachRelevantConnective((
        p1.combine(p2).combine(p3),  // All three args
        p2.combine(p1).combine(p3),  // All three args
        p3.combine(p1).combine(p2)   // All three args
      ))
    )
    assertEquals(result._1.s, "a + b + c")
    assertEquals(result._2.s, "b + a + c")
    assertEquals(result._3.s, "c + a + b")
  }

  test("NEGATIVE: ForEach-Relevant with multiple returns - one return missing an argument") {
    val obtained = compileErrors("""
      import ProductConnectiveFnsOps.*
      val pcTest1 = new ProductConnectiveFns("a")
      val pcTest2 = new ProductConnectiveFns("b")
      val pcTest3 = new ProductConnectiveFns("c")
      ProductConnectiveFns.forEachRelevantFn((pcTest1, pcTest2, pcTest3))((p1, p2, p3) =>
        ForEachRelevantConnective((p1.combine(p2).combine(p3), p1.combine(p2)))  // Error: second return missing p3
      )
    """)
    assert(
      obtained.contains(TestUtils.noGivenInstance),
      s"Expected ForEach-Relevant error but got: $obtained"
    )
  }