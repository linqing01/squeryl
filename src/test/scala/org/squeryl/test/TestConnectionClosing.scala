package org.squeryl.test

import org.squeryl.SessionFactory
import org.squeryl.framework.{DBConnector, DbTestBase}

abstract class ConnectionClosingTest extends DbTestBase {
  self: DBConnector =>

  def dbSpecificSelectNow: String

  test("Closing statement should close connection") {
    val session = SessionFactory.newSession
    val stmt = session.connection.prepareStatement(dbSpecificSelectNow)
    val rs = stmt.executeQuery

    session.connection.close()

    stmt.isClosed should equal(false)
    rs.isClosed should equal(false)
  }
}