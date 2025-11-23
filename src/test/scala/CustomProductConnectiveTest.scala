package test

import linearfn.{Multiplicity,ops, restricted, restrictedReturn, unrestricted}
import linearfn.RestrictedSelectable.*
import munit.FunSuite

@ops
class ProductConnectiveTest(val s: String):
  def higherOrderCombine(@restrictedReturn f: String => ProductConnectiveTest): ProductConnectiveTest =
    combine(f("hof"))
  def combine(other: ProductConnectiveTest): ProductConnectiveTest =
    new ProductConnectiveTest(s"${this.s} + ${other.s}")


type ForAllLinearConnective[RT <: Tuple, DT <: Tuple] = ComposedConnective[RT, DT, Multiplicity.Unrestricted, Multiplicity.Linear]
object ForAllLinearConnective:
  inline def apply[RT <: Tuple, DT <: Tuple]
    (values: RT)
    (using inline ev: DT =:= ExtractDependencyTypes[RT]): ForAllLinearConnective[RT, DT] =
    ComposedConnective[RT, DT, Multiplicity.Unrestricted, Multiplicity.Linear](values)

type ForEachLinearConnective[RT <: Tuple, DT <: Tuple] = ComposedConnective[RT, DT, Multiplicity.Linear, Multiplicity.Unrestricted]
object ForEachLinearConnective:
  inline def apply[RT <: Tuple, DT <: Tuple]
  (values: RT)
  (using inline ev: DT =:= ExtractDependencyTypes[RT]): ForEachLinearConnective[RT, DT] =
    ComposedConnective[RT, DT, Multiplicity.Linear, Multiplicity.Unrestricted](values)

object ProductConnectiveTest:
  def forAllLinearFn[QT <: Tuple, RT <: Tuple, DT <: Tuple](
    bases: QT
  )(fns: RestrictedFn.LinearFn[QT, ForAllLinearConnective[RT, DT]])(
    using builder: RestrictedFn.LinearFnBuilder[
      Multiplicity.Linear,
      QT,
      ForAllLinearConnective[RT, DT]
    ]
  ): ForAllLinearConnective[RT, DT] =
    builder.execute(bases)(fns)

  def forEachLinearFn[QT <: Tuple, RT <: Tuple, DT <: Tuple](
    bases: QT
  )(fns: RestrictedFn.LinearFn[QT, ForEachLinearConnective[RT, DT]])(
    using builder: RestrictedFn.LinearFnBuilder[
      Multiplicity.Linear,
      QT,
      ForEachLinearConnective[RT, DT]
    ]
  ): ForEachLinearConnective[RT, DT] =
    builder.execute(bases)(fns)

class CustomProductConnectiveTest extends FunSuite:
  import ProductConnectiveTestOps.*
  test("Custom Product Connective with for-all-linear") {
    val pcTest1 = new ProductConnectiveTest("a")
    val pcTest2 = new ProductConnectiveTest("b")
    val result = ProductConnectiveTest.forAllLinearFn((pcTest1, pcTest2))((p1, p2) =>
      ForAllLinearConnective((p1.combine(p2), p2))
    )
    val executed = result.execute()
    assertEquals(executed._1.s, "a + b")
    assertEquals(executed._2.s, "b")
  }
  test("Custom Product Connective with for-each-linear") {
    val pcTest1 = new ProductConnectiveTest("a")
    val pcTest2 = new ProductConnectiveTest("b")
    val result = ProductConnectiveTest.forEachLinearFn((pcTest1, pcTest2))((p1, p2) =>
      ForEachLinearConnective((p1.combine(p2), p1))
    )
    val executed = result.execute()
    assertEquals(executed._1.s, "a + b")
    assertEquals(executed._2.s, "a")
  }