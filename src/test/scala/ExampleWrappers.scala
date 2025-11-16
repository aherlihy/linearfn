package test

import linearfn.ops

@ops
class IntWrapper(val value: Int):
  def add(other: IntWrapper): IntWrapper =
    IntWrapper(this.value + other.value)

