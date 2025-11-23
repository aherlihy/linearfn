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