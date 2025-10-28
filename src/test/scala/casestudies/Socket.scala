package test.casestudies

import linearfn.{ops, consumed, unconsumed, repeatable}

/**
 * Case Study: Socket/Connection Protocol
 * Problem: Ensure proper connection lifecycle (connect → send/receive* → close)
 */

@ops
case class Socket(private var connected: Boolean, private val buffer: collection.mutable.ArrayBuffer[String]):
  /** Send message. Returns updated socket. */
  @repeatable
  def send(msg: String): Socket =
    if !connected then throw new IllegalStateException("Not connected")
    buffer += s"SENT: $msg"
    this

  /** Receive message without consuming socket. Returns (socket, message). */
  @unconsumed
  def receive(): (Socket, String) =
    if !connected then throw new IllegalStateException("Not connected")
    val msg = if buffer.nonEmpty then buffer.remove(0) else "EMPTY"
    (this, msg)

  /** Close connection. Consumes the socket and returns status. */
  @consumed
  def close(): String =
    if !connected then throw new IllegalStateException("Already closed")
    connected = false
    "Connection closed"

object Socket:
  def connect(host: String): Socket =
    Socket(true, collection.mutable.ArrayBuffer.empty)
