package org.squeryl.internals

import org.squeryl.Session
import org.squeryl.dsl.{ArrayJdbcMapper, TypedExpressionFactory}

import java.sql
import java.sql.ResultSet
import scala.reflect.ClassTag

class ArrayTEF[P, TE](toJdbc: P => Object, fromJdbc: Object => P)(implicit tt: ClassTag[P])
  extends TypedExpressionFactory[Array[P], TE] with ArrayJdbcMapper[java.sql.Array, Array[P]] {
  def sample: Array[P] = new Array[P](0)

  def toWrappedJDBCType(element: P): java.lang.Object = toJdbc(element)

  def fromWrappedJDBCType(element: Array[java.lang.Object]): Array[P] = element.map(fromJdbc)

  val defaultColumnLength = 1

  def extractNativeJdbcValue(rs: ResultSet, i: Int): sql.Array = rs.getArray(i)

  def convertToJdbc(v: Array[P]): java.sql.Array = {
    val content: Array[java.lang.Object] = v.map(toWrappedJDBCType)
    val s = Session.currentSession
    val con = s.connection
    var rv: java.sql.Array = null
    try {
      //asInstanceOf required for 2.9.0-1 to compile
      val typ = s.databaseAdapter.arrayCreationType(sample(0).asInstanceOf[ {def getClass: Class[_]}].getClass)
      rv = con.createArrayOf(typ, content)
    } catch {
      case e: Exception => s.log("Cannot create JDBC array: " + e.getMessage)
    }
    rv
  }

  def convertFromJdbc(v: java.sql.Array): Array[P] = {
    val s = Session.currentSession
    var rv: Array[P] = sample.take(0)
    try {
      val obj = v.getArray()
      rv = fromWrappedJDBCType(obj.asInstanceOf[Array[java.lang.Object]])
    } catch {
      case e: Exception => s.log("Cannot obtain array from JDBC: " + e.getMessage)
    }
    rv
  }
}