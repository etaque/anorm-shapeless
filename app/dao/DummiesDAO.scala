package dao

import models._

object DummiesDAO extends GenericDAO[Dummy] {
  val table = "dummies"

  val paramsEncoder = ParamsEncoder[Dummy]

  def testEncode() = println(encode(Dummy(Meta(1, 0), 2, "test", true, None)))
}
