package dao

import anorm._
import anorm.SqlParser._
import models._


object BrandsDAO extends AnormDAO[Brand] {
  val table = "brands"

  def makeRowParser(p: String) = for {
    meta <- Persisted.makeMetaParser(p)
    name <- get[String]("name")
    slug <- get[String]("permalink")
    status <- get[Int]("status")
    logo <- get[Option[String]]("logo")
  } yield Brand(meta, name, slug, status, logo)
}
