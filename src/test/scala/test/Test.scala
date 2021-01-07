package test

import org.squeryl.{KeyedEntity, PrimitiveTypeMode, Schema, Session, SessionFactory, Table}
import java.sql.Timestamp

case class Article
(
  var id: Long = 0,
  var title: String = "",
  var source: String = "",
  var author: String = "",
  var content: String = "",
  var tags: Option[String] = None,
  var type1: String = "",
  var type2: String = "",
  var type3: String = "",
  var position: String = "",
  var addtime: Option[java.sql.Date] = None,
  updated_at: Timestamp = null,
  created_at: Timestamp = null)
  extends KeyedEntity[Long] with PrimitiveTypeMode {
}

object TypeModes extends PrimitiveTypeMode

import TypeModes._

object Db extends Schema {
  val articles: Table[Article] = table[Article]("articles")
}

object Test {

  import org.squeryl.adapters.MySQLAdapter

  def main(args: Array[String]): Unit = {
    Class.forName("com.mysql.cj.jdbc.Driver")

    SessionFactory.concreteFactory = Some { () =>
      val url = "jdbc:mysql://127.0.0.1:3306/zjxh?useUnicode=true&characterEncoding=UTF-8&user=root&password="
      Session.create(java.sql.DriverManager.getConnection(url),
        new MySQLAdapter)
    }

    val session = SessionFactory.newSession

    session.bindToCurrentThread
    println(Db.articles.where(article => article.id === 124).head)
  }
}