package models

import java.time._
import scala.util.Try
import anorm._
import anorm.SqlParser._


trait Persisted {
  def meta: Persisted.Meta
  def id = meta.id
  def toParams: Seq[NamedParameter]
}

object Persisted {
  case class Meta(
    id: Long,
    createdAt: LocalDateTime,
    updatedAt: LocalDateTime
  ) {
    def toParams = Seq[NamedParameter](
      "id" -> id,
      "created_at" -> createdAt,
      "updated_at" -> updatedAt
    )
  }

  implicit val metaParser: RowParser[Meta] =
    Macro.parser[Meta]("id", "created_at", "updated_at")

  def makeMetaParser(prefix: String): RowParser[Meta] =
    Macro.parser[Meta](s"$prefix.id", s"$prefix.created_at", s"$prefix.updated_at")

  def newMeta = Meta(0, LocalDateTime.now, LocalDateTime.now)
}

trait HasSlug extends Persisted {
  def slug: String
  def name: String
  def idWithSlug = s"$id-$slug"

  def toSlugParams = Seq[NamedParameter](
    "name" -> name,
    "permalink" -> slug
  )
}

object HasSlug {
  def parseId(str: String): Option[Long] = {
    str.split("-").headOption.flatMap(rawId => Try(rawId.toLong).toOption)
  }
}
