package models

import anorm._
import shapeless._
import shapeless.record._
import shapeless.ops.record._
import shapeless.labelled.FieldType

case class Dummy(
  someInt: Int,
  someString: String
)


trait ParamsEncoder[A] {
  def encode(value: A): Seq[NamedParameter]
}

object ParamsEncoder {
  def apply[A](implicit enc: ParamsEncoder[A]): ParamsEncoder[A] = enc

  def createEncoder[A](fn: A => Seq[NamedParameter]): ParamsEncoder[A] =
    new ParamsEncoder[A] {
      def encode(value: A): Seq[NamedParameter] =
        fn(value)
    }

  implicit val hnilEncoder: ParamsEncoder[HNil] =
    createEncoder(hnil => Nil)

  implicit def hlistParamsEncoder[K <: Symbol, H, T <: HList](
    implicit
      witness: Witness.Aux[K],
    toSql: Lazy[ToSql[H]],
    toStmt: Lazy[ToStatement[H]],
    tEncoder: ParamsEncoder[T]
  ): ParamsEncoder[FieldType[K, H] :: T] = {
    val fieldName: String = witness.value.name
    createEncoder { hlist =>
      val value: H = hlist.head
      val head = ParameterValue.toParameterValue(value)(toSql.value, toStmt.value) //hEncoder.value.encode(hlist.head)
      val tail = tEncoder.encode(hlist.tail)
      NamedParameter(fieldName, head) +: tail
    }
  }

  implicit def genericParamsEncoder[A, H <: HList](
    implicit
      generic: LabelledGeneric.Aux[A, H],
    hEncoder: Lazy[ParamsEncoder[H]]
  ): ParamsEncoder[A] =
    createEncoder { value =>
      hEncoder.value.encode(generic.to(value))
    }
}

object TestDAO {
  val dummyGen = LabelledGeneric[Dummy]
  ParamsEncoder[Dummy].encode(Dummy(1, "test"))
  // def insert[A](value: A)(implicit pb: ParamBuilder[A]): Unit = {
  //   pb.toParam(value)
  // }

  // insert(Dummy(1, "test"))
}








// trait ParamBuilder[A] {
//   def toParam(value: A): Seq[NamedParameter]
// }

// object ParamBuilder {
//   def apply[A](implicit pb: ParamBuilder[A]): ParamBuilder[A] = pb
// }




// object npPoly extends Poly1 {
//   // implicit val intCase: Case.Aux[Int with shapeless.labelled.KeyTag[_, _], ParameterValue] =
//   //   at(ParameterValue.toParameterValue(_))

// }

// object pvPoly extends Poly1 {
//   implicit val intCase: Case.Aux[Int, ParameterValue] =
//     at(ParameterValue.toParameterValue(_))

//   implicit val stringCase: Case.Aux[String, ParameterValue] =
//     at(ParameterValue.toParameterValue(_))
// }

// object toName extends Poly1 {
//   implicit def keyToName[A] = at[Symbol with A](_.name)
// }

// object Dummy {
//   implicit val paramBuilder: ParamBuilder[Dummy] = new ParamBuilder[Dummy] {
//     def toParam(value: Dummy): Seq[NamedParameter] = {
//       // val p: String = LabelledGeneric[Dummy].to(value)
//       val lGen = LabelledGeneric[Dummy]
//       val repr = lGen.to(value)
//       repr.values.map(pvPoly)
//       Keys[lGen.Repr]

//       // LabelledGeneric[Dummy].to(value)
//       //   .toMap.toSeq.map { case (key, value) =>
//       //   // NamedParameter(key.name, value)
//       // }
//       ???

//     }
//     // Seq[NamedParameter](
//     //   'someInt -> value.someInt,
//     //   'someString -> value.someString
//     // )
//   }
// }

// object TestDAO {
//   def insert[A](value: A)(implicit pb: ParamBuilder[A]): Unit = {
//     pb.toParam(value)
//   }

//   insert(Dummy(1, "test"))
// }
