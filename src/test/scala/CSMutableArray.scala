package test

import linearfn.ops

import scala.reflect.ClassTag

final class MArrayUnsafe[A](private val buf: Array[A]) {
  def write(i: Int, a: A): MArrayUnsafe[A] = { buf(i) = a; this }
  def read(i: Int): A = buf(i)
  def freezeNoCopy(): Array[A] = buf
}

object MArrayUnsafe {
  def newMArray[A: ClassTag](size: Int): MArrayUnsafe[A] =
    new MArrayUnsafe(new Array[A](size))

  def array[A: ClassTag](size: Int, pairs: List[(Int, A)]): Array[A] = {
    val ma = newMArray[A](size)
    pairs.foreach { case (i, a) => ma.write(i, a) }
    ma.freezeNoCopy()
  }
}

@ops
final case class MArray[A](private val buf: Array[A]):
  def write(i: Int, a: A): MArray[A] =
    buf(i) = a; this

  /** Returns (arrayAgain, value). Value is unrestricted (plain A). */
  def read(i: Int): (MArray[A], A) =
    (this, buf(i))

  /** Consume the mutable array and return an immutable snapshot. */
  def freeze(): Array[A] =
    buf.clone()