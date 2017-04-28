package models

case class Dummy(
  meta: Meta,
  status: Status,
  someInt: Int,
  someString: String,
  someBool: Boolean,
  someOpt: Option[String]
)

case class Meta(
  id: Long,
  createdAt: Long
)

sealed trait Status { def code: String }
object Status {
  case object Pending extends Status { val code = "pending" }
  case object Active extends Status { val code = "active" }
}
