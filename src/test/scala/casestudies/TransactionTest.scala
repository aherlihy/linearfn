package test.casestudies

import munit.FunSuite
import linearfn.RestrictedSelectable
import scala.annotation.experimental
import test.TestUtils

/**
 * Tests for Transaction case study: Transaction Protocol
 *
 * Demonstrates: begin → insert/update* → commit/rollback pattern
 * Key: ONE LinearFn scope per test, applyConsumed ensures commit/rollback
 */
@experimental
class TransactionTest extends FunSuite:
  import TestUtils.*

  test("Transaction protocol: insert → commit (applyConsumed)") {
    import TransactionOps.*

    val tx = Transaction.begin()

    // applyConsumed: ensures commit() or rollback() is called
    val result = RestrictedSelectable.LinearFn.applyConsumed(Tuple1(tx))(refs =>
      val tx2 = refs._1.insert("users", "Alice, 30")
      val status = tx2.commit()  // @consumed
      Tuple1(status)
    )

    assert(result._1.contains("INSERT INTO users VALUES (Alice, 30)"))
  }

  test("Transaction protocol: insert → rollback (applyConsumed)") {
    import TransactionOps.*

    val tx = Transaction.begin()

    val result = RestrictedSelectable.LinearFn.applyConsumed(Tuple1(tx))(refs =>
      val tx2 = refs._1.insert("users", "Bob, 25")
      val status = tx2.rollback()  // @consumed
      Tuple1(status)
    )

    assertEquals(result._1, "ROLLED BACK")
  }

  test("Transaction protocol: multiple operations → commit") {
    import TransactionOps.*

    val tx = Transaction.begin()

    val result = RestrictedSelectable.LinearFn.applyConsumed(Tuple1(tx))(refs =>
      val tx2 = refs._1
        .insert("products", "Widget, 10")
        .update("products", "price = 20")
      val status = tx2.commit()  // @consumed
      Tuple1(status)
    )

    assert(result._1.contains("INSERT INTO products VALUES (Widget, 10)"))
    assert(result._1.contains("UPDATE products SET price = 20"))
  }

  test("NEGATIVE: transaction must be committed or rolled back") {
    import TransactionOps.*

    val obtained = compileErrors("""
      import TransactionOps.*
      import linearfn.RestrictedSelectable

      val tx = Transaction.begin()

      RestrictedSelectable.LinearFn.applyConsumed(Tuple1(tx))(refs =>
        val tx2 = refs._1.insert("users", "Data")
        Tuple1(tx2)  // Error: must call commit() or rollback()
      )
    """)

    assert(obtained.contains(consumptionExactlyOneMsg), s"Expected consumption error but got: $obtained")
  }

  test("NEGATIVE: cannot use transaction after commit") {
    import TransactionOps.*

    val obtained = compileErrors("""
      import TransactionOps.*
      import linearfn.RestrictedSelectable

      val tx = Transaction.begin()

      RestrictedSelectable.LinearFn.apply(Tuple1(tx))(refs =>
        val status = refs._1.commit().insert("users", "oops")  // Error: insert after @consumed commit
        Tuple1(status)
      )
    """)

    assert(obtained.contains(argsMsg), s"Expected args error but got: $obtained")
  }

  test("NEGATIVE: cannot use transaction after rollback") {
    import TransactionOps.*

    val obtained = compileErrors("""
      import TransactionOps.*
      import linearfn.RestrictedSelectable

      val tx = Transaction.begin()

      RestrictedSelectable.LinearFn.apply(Tuple1(tx))(refs =>
        val status = refs._1.rollback().insert("users", "oops")  // Error: insert after @consumed rollback
        Tuple1(status)
      )
    """)

    assert(obtained.contains(argsMsg), s"Expected args error but got: $obtained")
  }
