package test.casestudies

import munit.FunSuite
import linearfn.{RestrictedSelectable, VerticalConstraint, HorizontalConstraint}
import scala.annotation.experimental
import test.TestUtils

/**
 * Tests for FileHandle case study: File Handle Protocol
 *
 * Demonstrates: open → write* → close pattern with proper resource management
 */
@experimental
class FileHandleTest extends FunSuite:


  test("File protocol: write → close (applyConsumed ensures close)") {
    import FileHandleOps.*

    val file = FileHandle.open("test.txt")

    // customApply with Linear: ensures all arguments are consumed (close is called)
    val result = RestrictedSelectable.RestrictedFn.customApply(
      (vertical = VerticalConstraint.Linear, horizontal = HorizontalConstraint.ForAllRelevantForEachAffine)
    )(Tuple1(file))(refs =>
      val written = refs._1.write("Hello World")
      val status = written.close()  // @consumed
      Tuple1(status)
    )

    assertEquals(result._1, "File closed")
  }

  test("File protocol: multiple writes → close") {
    import FileHandleOps.*

    val file = FileHandle.open("test.txt")

    val result = RestrictedSelectable.RestrictedFn.customApply(
      (vertical = VerticalConstraint.Linear, horizontal = HorizontalConstraint.ForAllRelevantForEachAffine)
    )(Tuple1(file))(refs =>
      val file2 = refs._1.write("Line 1\n").write("Line 2\n").write("Line 3\n")
      val status = file2.close()  // @consumed
      Tuple1(status)
    )

    assertEquals(result._1, "File closed")
  }
  
// TODO: handle tuple return values and destructuring results  
//  test("File protocol: write → read → close (single LinearFn scope)") {
//    import FileHandleOps.*
//
//    val file = FileHandle.open("test.txt")
//
//    // Single LinearFn scope: read returns Restricted[(FileHandle, String), ...]
//    // We can't destructure inside, so we return the tuple then close
//    val result = RestrictedSelectable.RestrictedFn.applyConsumed(Tuple1(file))(refs =>
//      val written = refs._1.write("Hello World")
//      val readResult = written.read()  // @unconsumed: Restricted[(FileHandle, String), ...]
//      // readResult is a Restricted wrapper around a tuple
//      // We need to extract it and close - but we can't destructure inside LinearFn
//      // So we need a different approach: just close without reading the content
//      val status = written.close()  // close the file
//      Tuple1(status)
//    )
//
//    assertEquals(result._1, "File closed")
//  }

  test("NEGATIVE: file must be closed when using customApply with Linear") {
    import FileHandleOps.*

    val obtained = compileErrors("""
      import FileHandleOps.*
      import linearfn.{RestrictedSelectable, VerticalConstraint, HorizontalConstraint}

      val file = FileHandle.open("test.txt")

      RestrictedSelectable.RestrictedFn.customApply(
        (vertical = VerticalConstraint.Linear, horizontal = HorizontalConstraint.ForAllRelevantForEachAffine)
      )(Tuple1(file))(refs =>
        val written = refs._1.write("Data")
        Tuple1(written)  // Error: must consume (call close)
      )
    """)

    assert(obtained.contains(TestUtils.verticalConstraintFailed), s"Expected consumption error but got: $obtained")
  }

  test("NEGATIVE: cannot use file after close") {
    import FileHandleOps.*

    val obtained = compileErrors("""
      import FileHandleOps.*
      import linearfn.RestrictedSelectable

      val file = FileHandle.open("test.txt")

      RestrictedSelectable.RestrictedFn.apply(Tuple1(file))(refs =>
        val status = refs._1.close().write("oops")  // Error: write after @consumed close
        Tuple1(status)
      )
    """)

    assert(obtained.contains(TestUtils.missingField), s"Expected args error but got: $obtained")
  }
