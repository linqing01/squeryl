/** *****************************************************************************
 * Copyright 2010 Maxime Lévesque
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 * **************************************************************************** */
package org.squeryl.internals

import org.squeryl.Queryable
import org.squeryl.dsl.ast.{LogicalBoolean, QueryExpressionElements}
import org.squeryl.dsl.boilerplate.Query1
import org.squeryl.dsl.fsm.QueryElements

import java.sql.{Connection, ResultSet, SQLException, Statement}

object Utils {

  /**
   * Will attempt to evaluate a string expression and will catch any exception.
   * For use in circumstances when logging is needed (i.e. a fatal error has already occurred
   * and we need to log as much info as possible (i.e. put as much info as possible in the 'black box').
   * Also used to allow dumping (ex. for logging) a Query AST *before* it is completely built.
   */
  def failSafeString(s: => String): String =
    _failSafeString(() => s, "cannot evaluate")

  def failSafeString(s: => String, valueOnFail: String): String =
    _failSafeString(() => s, valueOnFail)

  private def _failSafeString(s: () => String, valueOnFail: String) =
    try {
      s()
    }
    catch {
      case e: Exception => valueOnFail
    }

  def close(s: Statement): Unit =
    try {
      s.close()
    }
    catch {
      case e: SQLException =>
    }

  def close(rs: ResultSet): Unit =
    try {
      rs.close()
    }
    catch {
      case e: SQLException =>
    }

  def close(c: Connection): Unit =
    try {
      c.close()
    }
    catch {
      case e: SQLException =>
    }

  private class DummyQueryElements[Cond](override val whereClause: Option[() => LogicalBoolean]) extends QueryElements[Cond]


  private class DummyQuery[A, B](q: Queryable[A], f: A => B, g: B => Unit) extends Query1[A, Int](
    q,
    a => {
      val res = f(a)
      g(res)
      new DummyQueryElements(None).select(0)
    },
    true,
    Nil)

  private class DummyQuery4WhereClause[A, B](q: Queryable[A], whereClause: A => LogicalBoolean) extends Query1[A, Int](
    q,
    a => {
      new DummyQueryElements(Some(() => whereClause(a))).select(0)
    },
    true,
    Nil)

  def createQuery4WhereClause[A](q: Queryable[A], whereClause: A => LogicalBoolean): QueryExpressionElements =
    new DummyQuery4WhereClause(q, whereClause).ast

  /**
   * visitor will get applied on a proxied Sample object of the Queryable[A],
   * this function is used for obtaining AST nodes or metadata from A.
   */
  def mapSampleObject[A, B](q: Queryable[A], visitor: A => B): B =
    FieldReferenceLinker.executeAndRestoreLastAccessedFieldReference {
      var b: Option[B] = None
      new DummyQuery(q, visitor, (b0: B) => b = Some(b0))
      b.get
    }

  def throwError(msg: String): Nothing = {
    throw new RuntimeException(msg)
  }


  def enumerationForValue(v: Enumeration#Value): Enumeration = {

    val m = v.getClass.getField("$outer")

    val enu = m.get(v).asInstanceOf[Enumeration]

    enu
  }
}

class IteratorConcatenation[R](first: Iterator[R], second: Iterator[R]) extends Iterator[R] {

  var currentIterator: Iterator[R] = first

  def _hasNext: Boolean =
    if (currentIterator.hasNext)
      true
    else if (currentIterator sameElements second)
      false
    else {
      currentIterator = second
      currentIterator.hasNext
    }

  def hasNext: Boolean = _hasNext

  def next(): R = {
    _hasNext
    currentIterator.next()
  }
}
