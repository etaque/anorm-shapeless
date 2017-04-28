package dao

import java.sql.Connection
import java.time._
import anorm._
import anorm.SqlParser._
import shapeless._
import shapeless.record._
import shapeless.ops.record._
import shapeless.labelled.FieldType


trait GenericDAO[T] {
  def table: String
  def encoder: ParamsEncoder[T]
  val idField = "id"

  def create(item: T)(implicit conn: Connection): Boolean = {
    val params = encoder.encode(item)
    val fields = params.map(_.name).filter(_ != idField)
    val placeholders = fields.map(n => s"{$n}").mkString(", ")

    SQL(s"INSERT INTO $table(${fields.mkString(", ")}) VALUES ($placeholders)")
      .on(params: _*)
      .execute()
  }

  def update(item: T)(implicit conn: Connection): Int = {
    val params = encoder.encode(item)

    val updates = params.map(_.name).filterNot(_ == idField).map { n =>
      s"$n = {$n}"
    }.mkString(", ")

    SQL(s"UPDATE $table SET $updates WHERE $idField = {$idField}")
      .on(params: _*)
      .executeUpdate()
  }
}

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
    toParamValue: Lazy[ToParameterValues[H]],
    tEncoder: ParamsEncoder[T]
  ): ParamsEncoder[FieldType[K, H] :: T] = {
    val fieldName: String = witness.value.name
    createEncoder { hlist =>
      val value: H = hlist.head
      val head = toParamValue.value(value)
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

trait ToParameterValues[A] {
  def apply(a: A): ParameterValue
}

object ToParameterValues {
  implicit def toParamValue[A](implicit s: ToSql[A] = null, p: ToStatement[A]) = new ToParameterValues[A]{
    def apply(a: A) = ParameterValue.toParameterValue[A](a)
  }
}

