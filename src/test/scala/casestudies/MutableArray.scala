package test

import restrictedfn.{ops}
import munit.FunSuite
import restrictedfn.RestrictedSelectable.RestrictedFn

import scala.reflect.ClassTag

@ops
final case class MArray[A: ClassTag](private val buf: Array[A]):
  def write(i: Int, a: A): MArray[A] =
    buf(i) = a; this

  /** Returns (arrayAgain, value). Value is unrestricted (plain A). */
  def read(i: Int): (MArray[A], A) =
    (this, buf(i))

  /** Combine this array with another array by concatenating them.
    * Returns a single MArray with dependencies on both inputs.
    * Can be returned twice to satisfy linearity (2 inputs → 2 outputs).
    * Point is to test methods that take arguments of the same type as self and return a value of that type.
    */
  def combine(other: MArray[A]): MArray[A] =
    val combined = new Array[A](buf.length + other.buf.length)
    Array.copy(buf, 0, combined, 0, buf.length)
    Array.copy(other.buf, 0, combined, buf.length, other.buf.length)
    MArray(combined)

  /** Query method that returns the size. */
  def size(): Int =
    buf.length

  /** No-op method that returns MArray. */
  def nothing(): MArray[A] =
    this

  /** Mark this array as sealed but keep it as MArray for testing. */
  def seal(): MArray[A] =
    this

  /** Consume the mutable array and return an immutable snapshot. */
  def freeze(): Array[A] =
    buf.clone()

