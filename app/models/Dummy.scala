package models

import anorm._
import shapeless._
import shapeless.record._

trait ParamBuilder[A] {
  def toParam(value: A): Seq[NamedParameter]
}

case class Dummy(
  someInt: Int,
  someString: String
)

object namedParameterPoly extends Poly2 {
  implicit val intCase: Case.Aux[Symbol, Int, NamedParameter] =
    at((k, v) => NamedParameter(k.name, ParameterValue.toParameterValue(v)))

  implicit val stringCase: Case.Aux[Symbol, String, NamedParameter] =
    at((k, v) => NamedParameter(k.name, ParameterValue.toParameterValue(v)))

  // implicit val stringCase: Case.Aux[String, ParameterValue] =
  //   at(ParameterValue.toParameterValue(_))
}

object Dummy {
  implicit val paramBuilder: ParamBuilder[Dummy] = new ParamBuilder[Dummy] {
    def toParam(value: Dummy): Seq[NamedParameter] = {
      val p: String = LabelledGeneric[Dummy].to(value)
      LabelledGeneric[Dummy].to(value).map(namedParameterPoly)

      // LabelledGeneric[Dummy].to(value)
      //   .toMap.toSeq.map { case (key, value) =>
      //   // NamedParameter(key.name, value)
      // }
      ???

    }
    // Seq[NamedParameter](
    //   'someInt -> value.someInt,
    //   'someString -> value.someString
    // )
  }
}

object TestDAO {
  def insert[A](value: A)(implicit pb: ParamBuilder[A]): Unit = {
    pb.toParam(value)
  }

  insert(Dummy(1, "test"))
}
