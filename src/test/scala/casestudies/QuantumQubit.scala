package test.casestudies

import linearfn.{ops, consumed, repeatable}

/**
 * Tiny quantum DSL to demonstrate function-local linearity:
 *
 * Quantum computing requires strict linearity because:
 * - No-cloning theorem: qubits cannot be duplicated
 * - Measurement collapses quantum state: after measurement, qubit is classical
 * - Unitary operations preserve quantum information
 *
 * Operations:
 * - hadamard: Applies Hadamard gate (creates superposition)
 * - x: Applies Pauli-X gate (quantum NOT)
 * - cnot: Controlled-NOT gate (entangles two qubits)
 * - measure: Collapses quantum state to classical bit (consumes qubit)
 */
@ops
final case class QuantumQubit(id: Int):
  /** Apply Hadamard gate - creates superposition from |0⟩ or |1⟩ */
  @repeatable
  def hadamard(): QuantumQubit = this

  /** Apply Pauli-X gate - quantum NOT operation */
  @repeatable
  def x(): QuantumQubit = this

  /**
   * Apply CNOT gate with this qubit as control, target as target.
   * Returns (control, target) both potentially entangled.
   */
  @repeatable
  def cnot(target: QuantumQubit): (QuantumQubit, QuantumQubit) = (this, target)

  /**
   * Measure qubit in computational basis.
   * Collapses quantum state to classical bit (0 or 1).
   * Marked @consumed because measured qubits cannot be reused as quantum state.
   */
  @consumed
  def measure(): Boolean = false

object QuantumQubit:
  private var next = 0
  def newQubit(init: Boolean): QuantumQubit =
    next += 1; QuantumQubit(next)
