package models

case class Dummy(
  meta: Meta,
  someInt: Int,
  someString: String,
  someBool: Boolean,
  someOpt: Option[String]
)

case class Meta(
  id: Long,
  createdAt: Long
)


