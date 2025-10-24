package test.casestudies

import linearfn.{ops, consumed}

/**
 * Case Study: Builder Pattern
 * Problem: Prevent using builder after build() is called
 */

@ops
case class CSQueryBuilder(private val parts: List[String]):
  /** Add SELECT clause. Returns new builder. */
  def select(fields: String): CSQueryBuilder =
    CSQueryBuilder(parts :+ s"SELECT $fields")

  /** Add FROM clause. Returns new builder. */
  def from(table: String): CSQueryBuilder =
    CSQueryBuilder(parts :+ s"FROM $table")

  /** Add WHERE clause. Returns new builder. */
  def where(condition: String): CSQueryBuilder =
    CSQueryBuilder(parts :+ s"WHERE $condition")

  /** Build final query string. Consumes the builder. */
  @consumed
  def build(): String =
    parts.mkString(" ")
