package restrictedfn

import restrictedfn.ops
import restrictedfn.RestrictedSelectable.{RestrictedFn, Restricted}
import scala.Tuple.*

// === Library-facing model (what users write) ===

//@ops // will generate ops (stageCall/stageField wrappers) for each method below
//final case class MArray[A](private val buf: Array[A]):
//  def write(i: Int, a: A): MArray[A] =
//    buf(i) = a; this
//
//  /** Returns (arrayAgain, value). Value is unrestricted (plain A). */
//  def read(i: Int): (MArray[A], A) =
//    (this, buf(i))
//
//  /** Consume the mutable array and return an immutable snapshot. */
//  def freeze(): Array[A] =
//    buf.clone()

//object MArray:
//  def newMArray[A](size: Int)(k: MArray[A] => Array[A]): Array[A] =
//    // The linear discipline is enforced by the call site; we just call k once.
//    k(MArray(new Array[A](size)))

// === Usage: construct an immutable array via linear single-threading ===
// build an Array[Int] from (index, value) pairs, like the paper’s `array`
//def array[A](size: Int, pairs: List[(Int, A)]): Array[A] =
//  MArray.newMArray[A](size) { ma0 =>
//    // foldl write ma pairs  |>  linear: the MArray is used exactly once through the fold
//    val frozen = RestrictedFn.apply(Tuple1(ma0)) { refs =>
//      val afterWrites =
//        pairs.foldLeft(refs._1) { (m, p) =>
//          // implicit lift: Int, A are treated as Restricted[..., EmptyTuple]
//          m.write(p._1, p._2)
//        }
//      Tuple1(afterWrites.freeze())
//    }
//    frozen._1
//  }
//
// ✓ OK: each MArray handle is threaded linearly

// ✗ Not OK (won't compile): would reuse the same handle twice
// RestrictedFn.apply(Tuple1(MArray(new Array)))(refs => (refs._1.write(0, 1), refs._1.freeze()))

@main def main() = {
  println("Linear Function Library")
//  val imm: Array[Int] = array(3, List(0 -> 10, 1 -> 20, 2 -> 30))

}
