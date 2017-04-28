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
  def paramsEncoder: ParamsEncoder[T]
  def naming: String => String = (s) => s
  val idField = "id"

  def encode(item: T): Seq[NamedParameter] = {
    paramsEncoder.encode(item) match {
      case AnormFinalValue(_) =>
        // dead branch
        Nil

      case AnormValueGroup(items) =>
        items.map { case (n, v) => NamedParameter(naming(n), v.value) }
    }
  }

  def create(item: T)(implicit conn: Connection): Boolean = {
    val params = encode(item)
    val fields = params.map(_.name).filter(_ != idField)
    val placeholders = fields.map(n => s"{$n}").mkString(", ")

    SQL(s"INSERT INTO $table(${fields.mkString(", ")}) VALUES ($placeholders)")
      .on(params: _*)
      .execute()
  }

  def update(item: T)(implicit conn: Connection): Int = {
    val params = encode(item)

    val updates = params.map(_.name).filterNot(_ == idField).map { n =>
      s"$n = {$n}"
    }.mkString(", ")

    SQL(s"UPDATE $table SET $updates WHERE $idField = {$idField}")
      .on(params: _*)
      .executeUpdate()
  }
}


sealed trait AnormValue
case class AnormFinalValue(value: ParameterValue) extends AnormValue
case class AnormValueGroup(items: Seq[(String, AnormFinalValue)]) extends AnormValue {
  def concat(other: AnormValue) = other match {
    case AnormValueGroup(otherItems) =>
      AnormValueGroup(items ++ otherItems)
    case AnormFinalValue(_) =>
      // dead branch
      this
  }
}

trait ParamsEncoder[A] {
  def encode(value: A): AnormValue
}

object ParamsEncoder {
  def apply[A](implicit enc: ParamsEncoder[A]): ParamsEncoder[A] = enc

  def createEncoder[A](fn: A => AnormValue): ParamsEncoder[A] =
    new ParamsEncoder[A] {
      def encode(value: A): AnormValue =
        fn(value)
    }

  def customEncoder[A, B](fn: A => B)(implicit bEncoder: ParamsEncoder[B]): ParamsEncoder[A] =
    createEncoder((a: A) => bEncoder.encode(fn(a)))

  implicit def valueEncoder[A](implicit s: ToSql[A] = null, p: ToStatement[A]) =
    createEncoder((a: A) => AnormFinalValue(ParameterValue.toParameterValue[A](a)))

  implicit val hnilEncoder: ParamsEncoder[HNil] =
    createEncoder(hnil => AnormValueGroup(Nil))

  implicit def hlistGroupEncoder[K <: Symbol, H, T <: HList](
    implicit
      witness: Witness.Aux[K],
    hEncoder: Lazy[ParamsEncoder[H]],
    tEncoder: ParamsEncoder[T]
  ): ParamsEncoder[FieldType[K, H] :: T] = {
    val fieldName: String = witness.value.name
    createEncoder { hlist =>
      val value: H = hlist.head
      val encodedHead = hEncoder.value.encode(value)
      val tail = tEncoder.encode(hlist.tail)
      encodedHead match {
        case fv: AnormFinalValue =>
          AnormValueGroup(Seq(fieldName -> fv)).concat(tail)
        case g: AnormValueGroup =>
          g.concat(tail)
      }
    }
  }

  implicit val cnilEncoder: ParamsEncoder[CNil] =
    createEncoder(cnil => AnormValueGroup(Nil))

  implicit def coproductEncoder[H, T <: Coproduct](
    implicit hEncoder: Lazy[ParamsEncoder[H]], tEncoder: ParamsEncoder[T]
  ): ParamsEncoder[H :+: T] =
    createEncoder {
      case Inl(h) =>
        hEncoder.value.encode(h)
      case Inr(t) =>
        tEncoder.encode(t)
    }

  implicit def genericEncoder[A, H <: HList](
    implicit
      generic: LabelledGeneric.Aux[A, H],
    hEncoder: Lazy[ParamsEncoder[H]]
  ): ParamsEncoder[A] =
    createEncoder { value =>
      hEncoder.value.encode(generic.to(value))
    }
}



