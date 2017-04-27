package models


case class Dummy(
  id: Long,
  someInt: Int,
  someString: String,
  someBool: Boolean,
  someOpt: Option[String]
)

// case class Sub(foo: String)


// object TestDAO {
//   def test() {
//     println("What: " + ParamsEncoder[Dummy].encode(Dummy(1, "test", true, None)))
//   }
// }

