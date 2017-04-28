package models

import java.time.LocalDateTime
import java.util.UUID

case class Dummy(
  meta: Meta,
  status: Status,
  someInt: Int,
  someString: String,
  someBool: Boolean,
  someOpt: Option[String],
  someSeq: Seq[String]
)

object Dummy {
  def spawn = Dummy(Meta(UUID.randomUUID, LocalDateTime.now), Status.Pending, 2, "test", true, None, Nil)
}

case class Meta(
  id: UUID,
  createdAt: LocalDateTime
)

sealed trait Status { def code: String }
object Status {
  case object Pending extends Status { val code = "pending" }
  case object Active extends Status { val code = "active" }
}
