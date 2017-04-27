package models

import anorm._
import anorm.SqlParser._


case class Brand(
  meta: Persisted.Meta,
  name: String,
  slug: String,
  status: Int,
  logo: Option[String]
) extends Persisted with HasSlug {
  def toParams = meta.toParams ++ toSlugParams ++ Seq[NamedParameter](
    "status" -> status,
    "logo" -> logo
  )
}

object Brand {
  sealed trait Status { def code: Int }
  case object Submitted { val code = 0 }
  case object Accepted { val code = 1 }
  case object Denied { val code = 2 }
}
