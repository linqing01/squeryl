package org.squeryl.dsl

import org.squeryl.Query
import org.squeryl.dsl.ast.ExpressionNode
import org.squeryl.internals.ResultSetMapper

import java.sql.ResultSet

class DelegateQuery[M](val q: Query[M]) extends Query[M] {

  def iterator: Iterator[M] = q.iterator

  def distinct: Query[M] = q.distinct

  def forUpdate: Query[M] = q.forUpdate

  def dumpAst: String = q.dumpAst

  def page(offset: Int, length: Int): Query[M] = q.page(offset, length)

  def statement: String = q.statement

  def ast: ExpressionNode = q.ast

  protected[squeryl] def invokeYield(rsm: ResultSetMapper, rs: ResultSet): M =
    q.invokeYield(rsm, rs)

  override private[squeryl] def copy(
                                      asRoot: Boolean,
                                      newUnions: List[(String, Query[M])]
                                    ): Query[M] =
    q.copy(asRoot, newUnions)

  def name: String = q.name

  private[squeryl] def give(rsm: ResultSetMapper, rs: ResultSet) =
    q.invokeYield(rsm, rs)

  def union(q0: Query[M]): Query[M] = q.union(q0)

  def unionAll(q0: Query[M]): Query[M] = q.unionAll(q0)

  def intersect(q0: Query[M]): Query[M] = q.intersect(q0)

  def intersectAll(q0: Query[M]): Query[M] = q.intersectAll(q0)

  def except(q0: Query[M]): Query[M] = q.except(q0)

  def exceptAll(q0: Query[M]): Query[M] = q.exceptAll(q0)
}
