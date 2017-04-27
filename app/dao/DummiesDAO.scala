package dao

import models._

object DummiesDAO extends GenericDAO[Dummy] {
  val table = "dummies"

  val encoder = ParamsEncoder[Dummy]
}
