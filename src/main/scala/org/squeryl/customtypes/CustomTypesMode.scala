/** *****************************************************************************
 * Copyright 2010 Maxime Lévesque
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 * **************************************************************************** */
package org.squeryl.customtypes


import org.squeryl.dsl._
import org.squeryl.internals.FieldMapper

import java.sql.Timestamp
import java.util.{Date, UUID}

trait CustomType[T] extends Product1[T] {
  def value: T

  def _1: T = value

  def canEqual(a: Any) = false
}

trait CustomTypesMode extends QueryDsl with FieldMapper {

  private[this] val ps = PrimitiveTypeSupport

  val stringTEF: NonPrimitiveJdbcMapper[String, StringField, TString] =
    new NonPrimitiveJdbcMapper[String, StringField, TString](ps.stringTEF, this) {
      def convertFromJdbc(v: String) = StringField(v)

      def convertToJdbc(v: StringField) = v.value
    }

  val optionStringTEF: TypedExpressionFactory[Option[StringField], TOptionString] with DeOptionizer[String, StringField, TString, Option[StringField], TOptionString] = new TypedExpressionFactory[Option[StringField], TOptionString] with DeOptionizer[String, StringField, TString, Option[StringField], TOptionString] {
    val deOptionizer = stringTEF
  }

  val dateTEF: NonPrimitiveJdbcMapper[Date, DateField, TDate] = new NonPrimitiveJdbcMapper[Date, DateField, TDate](ps.dateTEF, this) {
    def convertFromJdbc(v: Date) = DateField(v)

    def convertToJdbc(v: DateField) = v.value
  }

  val optionDateTEF: TypedExpressionFactory[Option[DateField], TOptionDate] with DeOptionizer[Date, DateField, TDate, Option[DateField], TOptionDate] = new TypedExpressionFactory[Option[DateField], TOptionDate] with DeOptionizer[Date, DateField, TDate, Option[DateField], TOptionDate] {
    val deOptionizer = dateTEF
  }

  val timestampTEF: NonPrimitiveJdbcMapper[Timestamp, TimestampField, TTimestamp] = new NonPrimitiveJdbcMapper[Timestamp, TimestampField, TTimestamp](ps.timestampTEF, this) {
    def convertFromJdbc(v: Timestamp) = TimestampField(v)

    def convertToJdbc(v: TimestampField) = v.value

  }

  val optionTimestampTEF: TypedExpressionFactory[Option[TimestampField], TOptionTimestamp] with DeOptionizer[Timestamp, TimestampField, TTimestamp, Option[TimestampField], TOptionTimestamp] = new TypedExpressionFactory[Option[TimestampField], TOptionTimestamp] with DeOptionizer[Timestamp, TimestampField, TTimestamp, Option[TimestampField], TOptionTimestamp] {
    val deOptionizer = timestampTEF
  }

  val booleanTEF: NonPrimitiveJdbcMapper[Boolean, BooleanField, TBoolean] = new NonPrimitiveJdbcMapper[Boolean, BooleanField, TBoolean](ps.booleanTEF, this) {
    def convertFromJdbc(v: Boolean) = BooleanField(v)

    def convertToJdbc(v: BooleanField) = v.value
  }

  val optionBooleanTEF: TypedExpressionFactory[Option[BooleanField], TOptionBoolean] with DeOptionizer[Boolean, BooleanField, TBoolean, Option[BooleanField], TOptionBoolean] = new TypedExpressionFactory[Option[BooleanField], TOptionBoolean] with DeOptionizer[Boolean, BooleanField, TBoolean, Option[BooleanField], TOptionBoolean] {
    val deOptionizer = booleanTEF
  }

  val uuidTEF: NonPrimitiveJdbcMapper[UUID, UuidField, TUUID] = new NonPrimitiveJdbcMapper[UUID, UuidField, TUUID](ps.uuidTEF, this) {
    def convertFromJdbc(v: UUID) = UuidField(v)

    def convertToJdbc(v: UuidField) = v.value
  }

  val optionUUIDTEF: TypedExpressionFactory[Option[UuidField], TOptionUUID] with DeOptionizer[UUID, UuidField, TUUID, Option[UuidField], TOptionUUID] = new TypedExpressionFactory[Option[UuidField], TOptionUUID] with DeOptionizer[UUID, UuidField, TUUID, Option[UuidField], TOptionUUID] {
    val deOptionizer = uuidTEF
  }
  // =========================== Numerical Floating Point ===========================

  val floatTEF: NonPrimitiveJdbcMapper[Float, FloatField, TFloat] with FloatTypedExpressionFactory[FloatField, TFloat] = new NonPrimitiveJdbcMapper[Float, FloatField, TFloat](ps.floatTEF, this) with FloatTypedExpressionFactory[FloatField, TFloat] {
    def convertFromJdbc(v: Float) = FloatField(v)

    def convertToJdbc(v: FloatField): Float = v.value
  }

  val optionFloatTEF: FloatTypedExpressionFactory[Option[FloatField], TOptionFloat] with DeOptionizer[Float, FloatField, TFloat, Option[FloatField], TOptionFloat] = new FloatTypedExpressionFactory[Option[FloatField], TOptionFloat] with DeOptionizer[Float, FloatField, TFloat, Option[FloatField], TOptionFloat] {
    val deOptionizer = floatTEF
  }

  val doubleTEF: NonPrimitiveJdbcMapper[Double, DoubleField, TDouble] with FloatTypedExpressionFactory[DoubleField, TDouble] = new NonPrimitiveJdbcMapper[Double, DoubleField, TDouble](ps.doubleTEF, this) with FloatTypedExpressionFactory[DoubleField, TDouble] {
    def convertFromJdbc(v: Double) = DoubleField(v)

    def convertToJdbc(v: DoubleField) = v.value
  }

  val optionDoubleTEF: FloatTypedExpressionFactory[Option[DoubleField], TOptionDouble] with DeOptionizer[Double, DoubleField, TDouble, Option[DoubleField], TOptionDouble] = new FloatTypedExpressionFactory[Option[DoubleField], TOptionDouble] with DeOptionizer[Double, DoubleField, TDouble, Option[DoubleField], TOptionDouble] {
    val deOptionizer = doubleTEF
  }

  val bigDecimalTEF: NonPrimitiveJdbcMapper[BigDecimal, BigDecimalField, TBigDecimal] with FloatTypedExpressionFactory[BigDecimalField, TBigDecimal] = new NonPrimitiveJdbcMapper[BigDecimal, BigDecimalField, TBigDecimal](ps.bigDecimalTEF, this) with FloatTypedExpressionFactory[BigDecimalField, TBigDecimal] {
    def convertFromJdbc(v: BigDecimal) = BigDecimalField(v)

    def convertToJdbc(v: BigDecimalField) = v.value
  }

  val optionBigDecimalTEF: FloatTypedExpressionFactory[Option[BigDecimalField], TOptionBigDecimal] with DeOptionizer[BigDecimal, BigDecimalField, TBigDecimal, Option[BigDecimalField], TOptionBigDecimal] = new FloatTypedExpressionFactory[Option[BigDecimalField], TOptionBigDecimal] with DeOptionizer[BigDecimal, BigDecimalField, TBigDecimal, Option[BigDecimalField], TOptionBigDecimal] {
    val deOptionizer = bigDecimalTEF
  }

  // =========================== Numerical Integral ===========================

  val byteTEF: NonPrimitiveJdbcMapper[Byte, ByteField, TByte] = new NonPrimitiveJdbcMapper[Byte, ByteField, TByte](ps.byteTEF, this) {
    def convertFromJdbc(v: Byte) = ByteField(v)

    def convertToJdbc(v: ByteField) = v.value
  }

  val optionByteTEF: IntegralTypedExpressionFactory[Option[ByteField], TOptionByte, Option[FloatField], TOptionFloat] with DeOptionizer[Byte, ByteField, TByte, Option[ByteField], TOptionByte] = new IntegralTypedExpressionFactory[Option[ByteField], TOptionByte, Option[FloatField], TOptionFloat] with DeOptionizer[Byte, ByteField, TByte, Option[ByteField], TOptionByte] {
    val deOptionizer = byteTEF
    val floatifyer: TypedExpressionFactory[Option[FloatField], TOptionFloat] = optionFloatTEF
  }

  val intTEF: NonPrimitiveJdbcMapper[Int, IntField, TInt] with IntegralTypedExpressionFactory[IntField, TInt, FloatField, TFloat] = new NonPrimitiveJdbcMapper[Int, IntField, TInt](ps.intTEF, this) with IntegralTypedExpressionFactory[IntField, TInt, FloatField, TFloat] {
    val floatifyer = floatTEF

    def convertFromJdbc(v: Int) = IntField(v)

    def convertToJdbc(v: IntField) = v.value
  }

  val optionIntTEF: IntegralTypedExpressionFactory[Option[IntField], TOptionInt, Option[FloatField], TOptionFloat] with DeOptionizer[Int, IntField, TInt, Option[IntField], TOptionInt] = new IntegralTypedExpressionFactory[Option[IntField], TOptionInt, Option[FloatField], TOptionFloat] with DeOptionizer[Int, IntField, TInt, Option[IntField], TOptionInt] {
    val deOptionizer: TypedExpressionFactory[IntField, TInt] with JdbcMapper[Int, IntField] = intTEF
    val floatifyer: TypedExpressionFactory[Option[FloatField], TOptionFloat] = optionFloatTEF
  }

  val longTEF: NonPrimitiveJdbcMapper[Long, LongField, TLong] with IntegralTypedExpressionFactory[LongField, TLong, DoubleField, TDouble] = new NonPrimitiveJdbcMapper[Long, LongField, TLong](ps.longTEF, this) with IntegralTypedExpressionFactory[LongField, TLong, DoubleField, TDouble] {
    val floatifyer = doubleTEF

    def convertFromJdbc(v: Long) = LongField(v)

    def convertToJdbc(v: LongField) = v.value
  }

  val optionLongTEF: IntegralTypedExpressionFactory[Option[LongField], TOptionLong, Option[DoubleField], TOptionDouble] with DeOptionizer[Long, LongField, TLong, Option[LongField], TOptionLong] = new IntegralTypedExpressionFactory[Option[LongField], TOptionLong, Option[DoubleField], TOptionDouble] with DeOptionizer[Long, LongField, TLong, Option[LongField], TOptionLong] {
    val deOptionizer: TypedExpressionFactory[LongField, TLong] with JdbcMapper[Long, LongField] = longTEF
    val floatifyer: TypedExpressionFactory[Option[DoubleField], TOptionDouble] = optionDoubleTEF
  }

  implicit def stringToTE(s: String): TypedExpression[StringField, TString] = stringTEF.createFromNativeJdbcValue(s)
  implicit def optionStringToTE(s: Option[String]): Option[StringField] = s.map(StringField)
  implicit def dateToTE(s: Date): TypedExpression[DateField, TDate] = dateTEF.createFromNativeJdbcValue(s)
  implicit def optionDateToTE(s: Option[Date]): Option[DateField] = s.map(DateField)
  implicit def timestampToTE(s: Timestamp): TypedExpression[TimestampField, TTimestamp] = timestampTEF.createFromNativeJdbcValue(s)
  implicit def optionTimestampToTE(s: Option[Timestamp]): Option[TimestampField] = s.map(TimestampField)
  implicit def booleanToTE(s: Boolean): TypedExpression[BooleanField, TBoolean] = booleanTEF.createFromNativeJdbcValue(s)
  implicit def optionBooleanToTE(s: Option[Boolean]): Option[BooleanField] = s.map(BooleanField)
  implicit def uuidToTE(s: UUID): TypedExpression[UuidField, TUUID] = uuidTEF.createFromNativeJdbcValue(s)
  implicit def optionUUIDToTE(s: Option[UUID]): Option[UuidField] = s.map(UuidField)
  implicit def byteToTE(f: Byte): TypedExpression[ByteField, TByte] = byteTEF.createFromNativeJdbcValue(f)
  implicit def optionByteToTE(f: Option[Byte]): Option[ByteField] = f.map(ByteField)
  implicit def intToTE(f: IntField): TypedExpression[IntField, TInt] = intTEF.create(f)
  implicit def optionIntToTE(f: Option[IntField]): TypedExpression[Option[IntField], TOptionInt] = optionIntTEF.create(f)
  //implicit def _intToTE(f: Int) = intTEF.createFromNativeJdbcValue(f)
  //implicit def _optionIntToTE(f: Option[Int]) = f.map(new IntField(_))
  implicit def longToTE(f: Long): TypedExpression[LongField, TLong] = longTEF.createFromNativeJdbcValue(f)
  implicit def optionLongToTE(f: Option[Long]): Option[LongField] = f.map(LongField)
  implicit def floatToTE(f: Float): TypedExpression[FloatField, TFloat] = floatTEF.createFromNativeJdbcValue(f)
  implicit def optionFloatToTE(f: Option[Float]): Option[FloatField] = f.map(FloatField)
  implicit def doubleToTE(f: Double): TypedExpression[DoubleField, TDouble] = doubleTEF.createFromNativeJdbcValue(f)
  implicit def optionDoubleToTE(f: Option[Double]): Option[DoubleField] = f.map(DoubleField)
  implicit def bigDecimalToTE(f: BigDecimal): TypedExpression[BigDecimalField, TBigDecimal] = bigDecimalTEF.createFromNativeJdbcValue(f)
  implicit def optionBigDecimalToTE(f: Option[BigDecimal]): Option[BigDecimalField] = f.map(BigDecimalField)
}

object CustomTypesMode extends CustomTypesMode


case class ByteField(value: Byte) extends CustomType[Byte]

case class IntField(value: Int) extends CustomType[Int]

case class StringField(value: String) extends CustomType[String]

case class DoubleField(value: Double) extends CustomType[Double]

case class BigDecimalField(value: BigDecimal) extends CustomType[BigDecimal]

case class FloatField(value: Float) extends CustomType[Float]

case class LongField(value: Long) extends CustomType[Long]

case class BooleanField(value: Boolean) extends CustomType[Boolean]

case class DateField(value: Date) extends CustomType[Date]

case class TimestampField(value: Timestamp) extends CustomType[Timestamp]

case class BinaryField(value: Array[Byte]) extends CustomType[Array[Byte]]

case class UuidField(value: UUID) extends CustomType[UUID]

