package test

import linearfn.{ops, consumed}

/**
 * Case Study: Transaction Protocol
 * Problem: Ensure transactions are committed or rolled back, not left open
 */

@ops
case class CSTransaction(private var open: Boolean, private val ops: collection.mutable.ArrayBuffer[String]):
  /** Insert operation. Returns updated transaction. */
  def insert(table: String, data: String): CSTransaction =
    if !open then throw new IllegalStateException("Transaction not open")
    ops += s"INSERT INTO $table VALUES ($data)"
    this

  /** Update operation. Returns updated transaction. */
  def update(table: String, data: String): CSTransaction =
    if !open then throw new IllegalStateException("Transaction not open")
    ops += s"UPDATE $table SET $data"
    this

  /** Commit transaction. Consumes the transaction and returns summary. */
  @consumed
  def commit(): String =
    if !open then throw new IllegalStateException("Transaction not open")
    open = false
    s"COMMITTED: ${ops.mkString("; ")}"

  /** Rollback transaction. Consumes the transaction and returns status. */
  @consumed
  def rollback(): String =
    if !open then throw new IllegalStateException("Transaction not open")
    open = false
    ops.clear()
    "ROLLED BACK"

object CSTransaction:
  def begin(): CSTransaction =
    CSTransaction(true, collection.mutable.ArrayBuffer.empty)
