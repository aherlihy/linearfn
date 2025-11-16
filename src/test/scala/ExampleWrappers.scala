package test

import linearfn.ops

@ops
class IntType(val value: Int):
  def add(other: IntType): IntType =
    IntType(this.value + other.value)

