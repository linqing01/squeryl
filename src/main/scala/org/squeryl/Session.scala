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
package org.squeryl

import org.squeryl.internals._
import org.squeryl.logging.StatisticsListener

import java.sql.{Connection, ResultSet, SQLException, Statement}
import scala.collection.mutable.ArrayBuffer
import scala.util.control.ControlThrowable


class LazySession(val connectionFunc: () => Connection, val databaseAdapter: DatabaseAdapter, val statisticsListener: Option[StatisticsListener] = None) extends AbstractSession {

  private[this] var _connection: Option[Connection] = None

  def hasConnection: Boolean = _connection.isDefined

  var originalAutoCommit = true

  var originalTransactionIsolation: Int = java.sql.Connection.TRANSACTION_NONE

  def connection: Connection = {
    /*
     * No need to synchronize this since Sessions are not thread safe
     */
    _connection getOrElse {
      val c = connectionFunc()
      try {
        originalAutoCommit = c.getAutoCommit
        if (originalAutoCommit)
          c.setAutoCommit(false)
        originalTransactionIsolation = c.getTransactionIsolation
        _connection = Option(c)
        c
      } catch {
        case e: SQLException =>
          Utils.close(connection)
          throw e
      }
    }
  }

  def withinTransaction[A](f: () => A): A = {
    var txOk = false
    try {
      val res = this.using[A](f)
      txOk = true
      res
    } catch {
      case e: ControlThrowable =>
        txOk = true
        throw e
    } finally {
      if (hasConnection) {
        try {
          try {
            if (txOk)
              connection.commit()
            else
              connection.rollback()
          } finally {
            if (originalAutoCommit != connection.getAutoCommit) {
              connection.setAutoCommit(originalAutoCommit)
            }
            connection.setTransactionIsolation(originalTransactionIsolation)
          }
        } catch {
          case e: SQLException =>
            Utils.close(connection)
            if (txOk) throw e // if an exception occurred b4 the commit/rollback we don't want to obscure the original exception
        }
        try {
          if (!connection.isClosed)
            connection.close()
        } catch {
          case e: SQLException =>
            if (txOk) throw e // if an exception occurred b4 the close we don't want to obscure the original exception
        }
      }
    }
  }

}

class Session(val connection: Connection, val databaseAdapter: DatabaseAdapter, val statisticsListener: Option[StatisticsListener] = None) extends AbstractSession {

  val hasConnection = true

  def withinTransaction[A](f: () => A): A = {
    val originalAutoCommit =
      try {
        val r = connection.getAutoCommit
        if (r)
          connection.setAutoCommit(false)
        r
      } catch {
        case e: SQLException =>
          Utils.close(connection)
          throw e
      }
    val originalTransactionIsolation =
      try {
        connection.getTransactionIsolation
      } catch {
        case e: SQLException =>
          Utils.close(connection)
          throw e
      }
    var txOk = false
    try {
      val res = this.using[A](f)
      txOk = true
      res
    } catch {
      case e: ControlThrowable =>
        txOk = true
        throw e
    } finally {
      try {
        try {
          if (txOk)
            connection.commit()
          else
            connection.rollback()
        } finally {
          if (originalAutoCommit != connection.getAutoCommit) {
            connection.setAutoCommit(originalAutoCommit)
          }
          connection.setTransactionIsolation(originalTransactionIsolation)
        }
      } catch {
        case e: SQLException =>
          Utils.close(connection)
          if (txOk) throw e // if an exception occurred b4 the commit/rollback we don't want to obscure the original exception
      }
      try {
        if (!connection.isClosed)
          connection.close()
      } catch {
        case e: SQLException =>
          if (txOk) throw e // if an exception occurred b4 the close we don't want to obscure the original exception
      }
    }
  }

}

trait AbstractSession {

  def connection: Connection

  def hasConnection: Boolean

  protected[squeryl] def withinTransaction[A](f: () => A): A

  protected[squeryl] def using[A](a: () => A): A = {
    val s = Session.currentSessionOption
    try {
      if (s.isDefined) s.get.unbindFromCurrentThread()
      try {
        this.bindToCurrentThread()
        val r = a()
        r
      }
      finally {
        this.unbindFromCurrentThread()
        this.cleanup()
      }
    }
    finally {
      if (s.isDefined) s.get.bindToCurrentThread()
    }
  }

  def databaseAdapter: DatabaseAdapter

  def statisticsListener: Option[StatisticsListener]

  def bindToCurrentThread(): Unit = Session.currentSession = Some(this)

  def unbindFromCurrentThread(): Unit = Session.currentSession = None

  private[this] var _logger: String => Unit = _

  def logger_=(f: String => Unit): Unit = _logger = f

  def setLogger(f: String => Unit): Unit = _logger = f

  def isLoggingEnabled: Boolean = _logger != null

  def log(s: String): Unit = if (isLoggingEnabled) _logger(s)

  var logUnclosedStatements = false

  private[this] val _statements = new ArrayBuffer[Statement]

  private[this] val _resultSets = new ArrayBuffer[ResultSet]

  private[squeryl] def _addStatement(s: Statement) = _statements.append(s)

  private[squeryl] def _addResultSet(rs: ResultSet) = _resultSets.append(rs)

  def cleanup(): Unit = {
    _statements.foreach(s => {
      if (logUnclosedStatements && isLoggingEnabled && !s.isClosed) {
        val stackTrace = Thread.currentThread.getStackTrace.map("at " + _).mkString("\n")
        log("Statement is not closed: " + s + ": " + System.identityHashCode(s) + "\n" + stackTrace)
      }
      Utils.close(s)
    })
    _statements.clear()
    _resultSets.foreach(rs => Utils.close(rs))
    _resultSets.clear()

    FieldReferenceLinker.clearThreadLocalState()
  }

  def close(): Unit = {
    cleanup()
    if (hasConnection)
      connection.close()
  }

}

trait SessionFactory {
  def newSession: AbstractSession
}

object SessionFactory {

  /**
   * Initializing concreteFactory with a Session creating closure enables the use of
   * the 'transaction' and 'inTransaction' block functions 
   */
  var concreteFactory: Option[() => AbstractSession] = None

  /**
   * Initializing externalTransactionManagementAdapter with a Session creating closure allows to
   * execute Squeryl statements *without* the need of using 'transaction' and 'inTransaction'.
   * The use case for this is to allow Squeryl connection/transactions to be managed by an
   * external framework. In this case Session.cleanupResources *needs* to be called when connections
   * are closed, otherwise statement of resultset leaks can occur. 
   */
  var externalTransactionManagementAdapter: Option[() => Option[AbstractSession]] = None

  def newSession: AbstractSession =
    concreteFactory.getOrElse(
      throw new IllegalStateException("org.squeryl.SessionFactory not initialized, SessionFactory.concreteFactory must be assigned a \n" +
        "function for creating new org.squeryl.Session, before transaction can be used.\n" +
        "Alternatively SessionFactory.externalTransactionManagementAdapter can initialized, please refer to the documentation.")
    ).apply()
}

object Session {

  /**
   * Note about ThreadLocals: all thread locals should be .removed() before the
   * transaction ends.
   *
   * Leaving a ThreadLocal inplace after the control returns to the user thread
   * will pollute the users threads and will cause problems for e.g. Tomcat and
   * other servlet engines.
   */
  private[this] val _currentSessionThreadLocal = new ThreadLocal[AbstractSession]

  def create(c: Connection, a: DatabaseAdapter) =
    new Session(c, a)

  def create(connectionFunc: () => Connection, a: DatabaseAdapter) =
    new LazySession(connectionFunc, a)

  def currentSessionOption: Option[AbstractSession] = {
    Option(_currentSessionThreadLocal.get) orElse {
      SessionFactory.externalTransactionManagementAdapter flatMap {
        _.apply()
      }
    }
  }

  def currentSession: AbstractSession =
    SessionFactory.externalTransactionManagementAdapter match {
      case Some(a) =>
        a.apply() getOrElse org.squeryl.internals.Utils.throwError("SessionFactory.externalTransactionManagementAdapter was unable to supply a Session for the current scope")
      case None =>
        currentSessionOption.getOrElse(
          throw new IllegalStateException("No session is bound to current thread, a session must be created via Session.create \nand bound to the thread via 'work' or 'bindToCurrentThread'\n Usually this error occurs when a statement is executed outside of a transaction/inTrasaction block"))
    }

  def hasCurrentSession: Boolean =
    currentSessionOption.isDefined

  def cleanupResources(): Unit =
    currentSessionOption foreach (_.cleanup())

  private[squeryl] def currentSession_=(s: Option[AbstractSession]): Unit =
    if (s.isEmpty) {
      _currentSessionThreadLocal.remove()
    } else {
      _currentSessionThreadLocal.set(s.get)
    }

}
