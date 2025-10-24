package test.casestudies

import linearfn.{ops, consumed, unconsumed}

/**
 * Case Study: Socket/Connection Protocol
 * Problem: Ensure proper connection lifecycle (connect → send/receive* → close)
 */

@ops
case class CSSocket(private var connected: Boolean, private val buffer: collection.mutable.ArrayBuffer[String]):
  /** Send message. Returns updated socket. */
  def send(msg: String): CSSocket =
    if !connected then throw new IllegalStateException("Not connected")
    buffer += s"SENT: $msg"
    this

  /** Receive message without consuming socket. Returns (socket, message). */
  @unconsumed
  def receive(): (CSSocket, String) =
    if !connected then throw new IllegalStateException("Not connected")
    val msg = if buffer.nonEmpty then buffer.remove(0) else "EMPTY"
    (this, msg)

  /** Close connection. Consumes the socket and returns status. */
  @consumed
  def close(): String =
    if !connected then throw new IllegalStateException("Already closed")
    connected = false
    "Connection closed"

object CSSocket:
  def connect(host: String): CSSocket =
    CSSocket(true, collection.mutable.ArrayBuffer.empty)
