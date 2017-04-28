package dao

import models._

object DummiesDAO extends GenericDAO[Dummy] {
  val table = "dummies"

  implicit val statusEncoder = ParamsEncoder.customEncoder[Status, String](_.code)
  val paramsEncoder = ParamsEncoder[Dummy]

  def testEncode() = println(encode(Dummy(Meta(1, 0), Status.Pending, 2, "test", true, None)))
}
