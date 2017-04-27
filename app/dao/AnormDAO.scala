package dao

import java.sql.Connection
import java.time._
import anorm._
import anorm.SqlParser._
import models.Persisted


trait AnormDAO[T <: Persisted] {

  def table: String

  def makeRowParser(prefix: String): RowParser[T]
  def rowParser = makeRowParser(table)

  val idField = "id"

  def customUpdatePlaceholders = Map.empty[String, (String => String)]

  def create(item: T)(implicit conn: Connection): Boolean = {
    val params = item.toParams
    val fields = params.map(_.name).filter(_ != idField)
    val placeholders = fields.map(n => s"{$n}").mkString(", ")

    SQL(s"INSERT INTO $table(${fields.mkString(", ")}) VALUES ($placeholders)")
      .on(params: _*)
      .execute()
  }

  def find(id: Long)(implicit conn: Connection): Option[T] =
    SQL"SELECT * FROM #$table WHERE #$idField = $id"
      .as(rowParser.singleOpt)

  def findOrThrow(id: Long)(implicit conn: Connection): T =
    find(id).getOrElse(sys.error(s"Expected row with $idField = $id in table $table"))

  def findBy[A](field: String, value: A)(implicit conn: Connection, toStmt: ToStatement[A]): Option[T] =
    SQL"SELECT * FROM #$table WHERE #$field = $value"
      .as(rowParser.singleOpt)

  def findAll[A](ids: Seq[Long])(implicit conn: Connection): Seq[T] =
    if (ids.isEmpty)
      Nil
    else
      SQL"SELECT * FROM #$table WHERE #$idField IN ($ids)"
        .as(rowParser.*)

  def exists(id: Long)(implicit conn: Connection): Boolean =
    SQL"SELECT COUNT(*) > 0 FROM #$table WHERE #$idField = $id"
      .as(SqlParser.scalar[Boolean].single)

  def listBy[A](field: String, value: A)(implicit conn: Connection, toStmt: ToStatement[A]): Seq[T] =
    SQL"SELECT * FROM #$table WHERE #$field = $value"
      .as(rowParser.*)

  def listAll()(implicit conn: Connection): Seq[T] =
    SQL"SELECT * FROM #$table order by #$idField"
      .as(rowParser.*)

  def update(item: T)(implicit conn: Connection): Int = {
    val params = item.toParams

    val updates = params.map(_.name).filterNot(_ == idField).map { n =>
      val placeholder = customUpdatePlaceholders.get(n).map(_(n)).getOrElse(s"{$n}")
      s"$n = $placeholder"
    }.mkString(", ")

    SQL(s"UPDATE $table SET $updates WHERE $idField = {$idField}")
      .on(params: _*)
      .executeUpdate()
  }

  def updateBy[A](id: Long)(field: String, value: A)(implicit conn: Connection, toStmt: ToStatement[A], toStmtId: ToStatement[Long]): Int =
    SQL"UPDATE #$table SET #$field = $value WHERE #$idField = $id"
      .executeUpdate()

  def delete(id: Long)(implicit conn: Connection): Boolean =
    SQL"DELETE FROM #$table WHERE #$idField = $id"
      .execute()

  def batchCreate(items: Seq[T])(implicit conn: Connection): Unit =
    items.toList match {
      case (head :: xs) => {

        val params = head.toParams
        val fields = params.map(_.name)

        val queryFields = fields.mkString(", ")
        val queryCmd = s"INSERT INTO $table($queryFields) VALUES "

        val allPlaceholders = items.zipWithIndex.map { case (item, i) =>
          "(" + fields.map(n => s"{${n}_$i}").mkString(", ") + ")"
        }.mkString(", ")

        val allParams = items.zipWithIndex.flatMap { case (item, i) =>
          item.toParams.map(np => np.copy(name = s"${np.name}_$i"))
        }

        SQL(queryCmd + allPlaceholders)
          .on(allParams: _*)
          .execute()
      }
      case _ => () // empty list, do nothing
    }
}
