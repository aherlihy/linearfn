package test.casestudies

import linearfn.{ops, consumed, unconsumed}

/**
 * Case Study: File Handle Protocol
 * Problem: Prevent reading/writing after close, ensure files are eventually closed
 */

case class FileData(content: String)

@ops
case class CSFileHandle(private var data: FileData, private var closed: Boolean = false):
  /** Write content to file. Returns new handle. */
  def write(content: String): CSFileHandle =
    if closed then throw new IllegalStateException("File already closed")
    data = FileData(data.content + content)
    this

  /** Read file content without consuming the handle. Returns (handle, content). */
  @unconsumed
  def read(): (CSFileHandle, String) =
    if closed then throw new IllegalStateException("File already closed")
    (this, data.content)

  /** Close the file. Consumes the handle and returns status. */
  @consumed
  def close(): String =
    if closed then throw new IllegalStateException("Already closed")
    closed = true
    "File closed"

object CSFileHandle:
  def open(path: String): CSFileHandle =
    CSFileHandle(FileData(""))
