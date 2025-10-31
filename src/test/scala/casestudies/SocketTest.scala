package test.casestudies

import munit.FunSuite
import linearfn.{RestrictedSelectable, VerticalConstraint, HorizontalConstraint}
import scala.annotation.experimental
import test.TestUtils

/**
 * Tests for Socket case study: Socket/Connection Protocol
 *
 * Demonstrates: connect → send/receive* → close pattern
 * Key: ONE LinearFn scope per test, applyConsumed ensures close is called
 */
@experimental
class SocketTest extends FunSuite:


  test("Socket protocol: send → close (customApply with Linear ensures close)") {
    import SocketOps.*

    val socket = Socket.connect("localhost:8080")

    // customApply with Linear: ensures close() is called
    val result = RestrictedSelectable.RestrictedFn.customApply(
      (vertical = VerticalConstraint.Linear, horizontal = HorizontalConstraint.ForAllRelevantForEachAffine)
    )(Tuple1(socket))(refs =>
      val sent = refs._1.send("Hello")
      val status = sent.close()  // @consumed
      Tuple1(status)
    )

    assertEquals(result._1, "Connection closed")
  }

  test("Socket protocol: multiple sends then close") {
    import SocketOps.*

    val socket = Socket.connect("localhost:8080")

    val result = RestrictedSelectable.RestrictedFn.customApply(
      (vertical = VerticalConstraint.Linear, horizontal = HorizontalConstraint.ForAllRelevantForEachAffine)
    )(Tuple1(socket))(refs =>
      val sent = refs._1.send("Msg1").send("Msg2").send("Msg3")
      val status = sent.close()  // @consumed
      Tuple1(status)
    )

    assertEquals(result._1, "Connection closed")
  }

// TODO: handle tuple return values and deconstructing results
//  test("Socket protocol: send → receive → close (single scope)") {
//    import SocketOps.*
//
//    val socket = Socket.connect("localhost:8080")
//
//    // Single LinearFn scope: receive returns Restricted[(Socket, String), ...]
//    // We close without extracting the message content
//    val result = RestrictedSelectable.RestrictedFn.applyConsumed(Tuple1(socket))(refs =>
//      val sent = refs._1.send("Hello")
//      val receiveResult = sent.receive()  // @unconsumed: Restricted[(Socket, String), ...]
//      // Can't destructure inside, so just close
//      val status = sent.close()  // @consumed
//      Tuple1(status)
//    )
//
//    assertEquals(result._1, "Connection closed")
//  }

  test("NEGATIVE: socket must be closed when using customApply with Linear") {
    import SocketOps.*

    val obtained = compileErrors("""
      import SocketOps.*
      import linearfn.{RestrictedSelectable, VerticalConstraint, HorizontalConstraint}

      val socket = Socket.connect("localhost:8080")

      RestrictedSelectable.RestrictedFn.customApply(
        (vertical = VerticalConstraint.Linear, horizontal = HorizontalConstraint.ForAllRelevantForEachAffine)
      )(Tuple1(socket))(refs =>
        val sent = refs._1.send("Data")
        Tuple1(sent)  // Error: must consume (call close)
      )
    """)

    assert(obtained.contains(TestUtils.verticalConstraintFailed), s"Expected consumption error but got: $obtained")
  }

  test("NEGATIVE: cannot use socket after close") {
    import SocketOps.*

    val obtained = compileErrors("""
      import SocketOps.*
      import linearfn.RestrictedSelectable

      val socket = Socket.connect("localhost:8080")

      RestrictedSelectable.RestrictedFn.apply(Tuple1(socket))(refs =>
        val status = refs._1.close().send("oops")  // Error: send after @consumed close
        Tuple1(status)
      )
    """)

    assert(obtained.contains(TestUtils.missingField), s"Expected args error but got: $obtained")
  }
