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

package org.squeryl.internals

import org.squeryl.dsl.{ArrayJdbcMapper, _}

import java.sql
import java.sql.{ResultSet, Timestamp}
import java.util.{Date, UUID}
import scala.annotation.tailrec
import scala.collection.mutable

trait FieldMapper {
  outer =>

  private[this] val registry = new mutable.HashMap[Class[_], FieldAttributesBasedOnType[_]]

  implicit def thisFieldMapper: FieldMapper = this

  /**
   * Extending classes will expose members of PrimitiveTypeSupport as implicit, to enable
   * support of primitive types, or will expose theit own non jdbc native types. 
   */

  protected object PrimitiveTypeSupport {
    // =========================== Non Numerical =========================== 

    val stringTEF: TEF[String, TString] = TEF("", 128, _.getString)
    val optionStringTEF: TEFO[String, TString, TOptionString] = TEFO(stringTEF)

    val dateTEF: TEF[Date, TDate] = TEF(new Date(), -1, _.getDate)
    val sqlDateTEF: TEF[sql.Date, TDate] = TEF(new sql.Date(0L), -1, _.getDate)

    val optionDateTEF: TEFO[Date, TDate, TOptionDate] = TEFO(dateTEF)
    val optionSqlDateTEF: TEFO[sql.Date, TDate, TOptionDate] = TEFO(sqlDateTEF)

    val timestampTEF: TEF[Timestamp, TTimestamp] = TEF(new Timestamp(0), -1, _.getTimestamp)
    val optionTimestampTEF: TEFO[Timestamp, TTimestamp, TOptionTimestamp] = TEFO(timestampTEF)

    val booleanTEF: TEF[Boolean, TBoolean] = TEF(true, 1, _.getBoolean)
    val optionBooleanTEF: TEFO[Boolean, TBoolean, TOptionBoolean] = TEFO(booleanTEF)

    val uuidTEF: TEF[UUID, TUUID] = {
      val uuid = java.util.UUID.fromString("00000000-0000-0000-0000-000000000000")
      TEF(uuid, 36, { rs =>
        i =>
          val v = rs.getObject(i)
          v match {
            case u: UUID => u
            case s: String => UUID.fromString(s)
            case _ => uuid
          }
      })
    }
    val optionUUIDTEF: TEFO[UUID, TUUID, TOptionUUID] = TEFO(uuidTEF)

    val binaryTEF: TEF[Array[Byte], TByteArray] = TEF(Array[Byte](0), 255, _.getBytes)

    val optionByteArrayTEF: TypedExpressionFactory[Option[Array[Byte]], TOptionByteArray] with DeOptionizer[Array[Byte], Array[Byte], TByteArray, Option[Array[Byte]], TOptionByteArray] = new TypedExpressionFactory[Option[Array[Byte]], TOptionByteArray] with DeOptionizer[Array[Byte], Array[Byte], TByteArray, Option[Array[Byte]], TOptionByteArray] {
      val deOptionizer = binaryTEF
    }

    val intArrayTEF: ArrayTEF[Int, TIntArray] = new ArrayTEF(java.lang.Integer.valueOf, _.asInstanceOf[Integer].toInt)
    val longArrayTEF: ArrayTEF[Long, TLongArray] = new ArrayTEF(java.lang.Long.valueOf, _.asInstanceOf[Long])
    val doubleArrayTEF: ArrayTEF[Double, TDoubleArray] = new ArrayTEF(java.lang.Double.valueOf, _.asInstanceOf[Double])
    val stringArrayTEF: ArrayTEF[String, TStringArray] = new ArrayTEF(new java.lang.String(_), _.asInstanceOf[String])

    // FIXME: The type soup on this was beyond my patience for now...I think we'll need an ArrayDeOptionizer
    //val optionIntArrayTEF = new TypedExpressionFactory[Option[Array[Int]],TOptionIntArray] with DeOptionizer[Array[Int], Array[Int], TIntArray, Option[Array[Int]], TOptionIntArray] {
    //val deOptionizer = intArrayTEF
    //}

    def enumValueTEF[A >: Enumeration#Value <: Enumeration#Value](ev: Enumeration#Value): JdbcMapper[Int, A] with TypedExpressionFactory[A, TEnumValue[A]] =
      new JdbcMapper[Int, A] with TypedExpressionFactory[A, TEnumValue[A]] {

        val enu = Utils.enumerationForValue(ev)

        def extractNativeJdbcValue(rs: ResultSet, i: Int) = rs.getInt(i)

        def defaultColumnLength: Int = intTEF.defaultColumnLength

        def sample: A = ev

        def convertToJdbc(v: A) = v.id

        def convertFromJdbc(v: Int) = {
          enu.values.find(_.id == v).getOrElse(DummyEnum.DummyEnumerationValue) // JDBC has no concept of null value for primitive types (ex. Int)
          // at this level, we mimic this JDBC flaw (the Option / None based on jdbc.wasNull will get sorted out by optionEnumValueTEF)
        }
      }

    object DummyEnum extends Enumeration {
      type DummyEnum = Value
      val DummyEnumerationValue: PrimitiveTypeSupport.DummyEnum.Value = Value(-1, "DummyEnumerationValue")
    }

    def optionEnumValueTEF[A >: Enumeration#Value <: Enumeration#Value](ev: Option[Enumeration#Value]): TypedExpressionFactory[Option[A], TOptionEnumValue[A]] with DeOptionizer[Int, A, TEnumValue[A], Option[A], TOptionEnumValue[A]] = new TypedExpressionFactory[Option[A], TOptionEnumValue[A]] with DeOptionizer[Int, A, TEnumValue[A], Option[A], TOptionEnumValue[A]] {
      val deOptionizer: TypedExpressionFactory[A, TEnumValue[A]] with JdbcMapper[Int, A] = {
        val e = ev.getOrElse(PrimitiveTypeSupport.DummyEnum.DummyEnumerationValue)
        enumValueTEF[A](e)
      }
    }

    // =========================== Numerical Integral =========================== 

    val byteTEF: IntegralTEF[Byte, TByte, Float, TFloat] = IntegralTEF(1, 1, _.getByte, floatTEF)

    val optionByteTEF: IntegralTypedExpressionFactory[Option[Byte], TOptionByte, Option[Float], TOptionFloat] with DeOptionizer[Byte, Byte, TByte, Option[Byte], TOptionByte] = new IntegralTypedExpressionFactory[Option[Byte], TOptionByte, Option[Float], TOptionFloat] with DeOptionizer[Byte, Byte, TByte, Option[Byte], TOptionByte] {
      val deOptionizer: TypedExpressionFactory[Byte, TByte] with JdbcMapper[Byte, Byte] = byteTEF
      val floatifyer: TypedExpressionFactory[Option[Float], TOptionFloat] = optionFloatTEF
    }

    val intTEF: IntegralTypedExpressionFactory[Int, TInt, Float, TFloat] with PrimitiveJdbcMapper[Int] = new IntegralTypedExpressionFactory[Int, TInt, Float, TFloat] with PrimitiveJdbcMapper[Int] {
      val sample = 1
      val defaultColumnLength = 4
      val floatifyer = floatTEF

      def extractNativeJdbcValue(rs: ResultSet, i: Int) = rs.getInt(i)
    }

    val optionIntTEF: IntegralTypedExpressionFactory[Option[Int], TOptionInt, Option[Float], TOptionFloat] with DeOptionizer[Int, Int, TInt, Option[Int], TOptionInt] = new IntegralTypedExpressionFactory[Option[Int], TOptionInt, Option[Float], TOptionFloat] with DeOptionizer[Int, Int, TInt, Option[Int], TOptionInt] {
      val deOptionizer: TypedExpressionFactory[Int, TInt] with JdbcMapper[Int, Int] = intTEF
      val floatifyer: TypedExpressionFactory[Option[Float], TOptionFloat] = optionFloatTEF
    }

    val longTEF: IntegralTypedExpressionFactory[Long, TLong, Double, TDouble] with PrimitiveJdbcMapper[Long] = new IntegralTypedExpressionFactory[Long, TLong, Double, TDouble] with PrimitiveJdbcMapper[Long] {
      val sample = 1L
      val defaultColumnLength = 8
      val floatifyer = doubleTEF

      def extractNativeJdbcValue(rs: ResultSet, i: Int) = rs.getLong(i)
    }

    val optionLongTEF: IntegralTypedExpressionFactory[Option[Long], TOptionLong, Option[Double], TOptionDouble] with DeOptionizer[Long, Long, TLong, Option[Long], TOptionLong] = new IntegralTypedExpressionFactory[Option[Long], TOptionLong, Option[Double], TOptionDouble] with DeOptionizer[Long, Long, TLong, Option[Long], TOptionLong] {
      val deOptionizer: TypedExpressionFactory[Long, TLong] with JdbcMapper[Long, Long] = longTEF
      val floatifyer: TypedExpressionFactory[Option[Double], TOptionDouble] = optionDoubleTEF
    }

    // =========================== Numerical Floating Point =========================== 

    val floatTEF: FloatTypedExpressionFactory[Float, TFloat] with PrimitiveJdbcMapper[Float] = new FloatTypedExpressionFactory[Float, TFloat] with PrimitiveJdbcMapper[Float] {
      val sample = 1F
      val defaultColumnLength = 4

      def extractNativeJdbcValue(rs: ResultSet, i: Int) = rs.getFloat(i)
    }

    val optionFloatTEF: FloatTypedExpressionFactory[Option[Float], TOptionFloat] with DeOptionizer[Float, Float, TFloat, Option[Float], TOptionFloat] = new FloatTypedExpressionFactory[Option[Float], TOptionFloat] with DeOptionizer[Float, Float, TFloat, Option[Float], TOptionFloat] {
      val deOptionizer = floatTEF
    }

    val doubleTEF: FloatTypedExpressionFactory[Double, TDouble] with PrimitiveJdbcMapper[Double] = new FloatTypedExpressionFactory[Double, TDouble] with PrimitiveJdbcMapper[Double] {
      val sample = 1D
      val defaultColumnLength = 8

      def extractNativeJdbcValue(rs: ResultSet, i: Int) = rs.getDouble(i)
    }

    val optionDoubleTEF: FloatTypedExpressionFactory[Option[Double], TOptionDouble] with DeOptionizer[Double, Double, TDouble, Option[Double], TOptionDouble] = new FloatTypedExpressionFactory[Option[Double], TOptionDouble] with DeOptionizer[Double, Double, TDouble, Option[Double], TOptionDouble] {
      val deOptionizer = doubleTEF
    }

    val bigDecimalTEF: FloatTypedExpressionFactory[BigDecimal, TBigDecimal] with PrimitiveJdbcMapper[BigDecimal] = new FloatTypedExpressionFactory[BigDecimal, TBigDecimal] with PrimitiveJdbcMapper[BigDecimal] {
      val sample = BigDecimal(1)
      val defaultColumnLength = -1

      def extractNativeJdbcValue(rs: ResultSet, i: Int) = {
        val v = rs.getBigDecimal(i)
        if (rs.wasNull())
          null
        else
          BigDecimal(v)
      }
    }

    val optionBigDecimalTEF: FloatTypedExpressionFactory[Option[BigDecimal], TOptionBigDecimal] with DeOptionizer[BigDecimal, BigDecimal, TBigDecimal, Option[BigDecimal], TOptionBigDecimal] = new FloatTypedExpressionFactory[Option[BigDecimal], TOptionBigDecimal] with DeOptionizer[BigDecimal, BigDecimal, TBigDecimal, Option[BigDecimal], TOptionBigDecimal] {
      val deOptionizer = bigDecimalTEF
    }
  }

  initialize()

  protected def initialize(): Option[FieldAttributesBasedOnType[_]] = {
    import PrimitiveTypeSupport._

    register(byteTEF)
    register(intTEF)
    register(longTEF)
    register(floatTEF)
    register(doubleTEF)
    register(bigDecimalTEF)

    register(binaryTEF)
    register(booleanTEF)
    register(stringTEF)
    register(timestampTEF)
    register(dateTEF)
    register(sqlDateTEF)
    register(uuidTEF)
    register(intArrayTEF)
    register(longArrayTEF)
    register(doubleArrayTEF)
    register(stringArrayTEF)

    val re: JdbcMapper[Int, Enumeration#Value] with TypedExpressionFactory[Enumeration#Value, TEnumValue[Enumeration#Value]] = enumValueTEF(DummyEnum.DummyEnumerationValue)

    /**
     * Enumerations are treated differently, since the map method should normally
     * return the actual Enumeration#value, but given that an enum is not only
     * determined by the int value from the DB, but also the parent Enumeration
     * parentEnumeration.values.find(_.id == v), the conversion is done 
     * in FieldMetaData.canonicalEnumerationValueFor(i: Int) 
     */
    val z = new FieldAttributesBasedOnType[Any](
      new {
        def map(rs: ResultSet, i: Int) = rs.getInt(i)

        def convertToJdbc(v: AnyRef) = v
      },
      re.defaultColumnLength,
      re.sample,
      classOf[java.lang.Integer])

    registry.put(z.clasz, z)
    registry.put(z.clasz.getSuperclass, z)
  }

  protected type MapperForReflection = {
    def map(rs: ResultSet, i: Int): Any
    def convertToJdbc(v: AnyRef): AnyRef
  }

  protected def makeMapper(fa0: JdbcMapper[_, _]): Object {
    val fa: JdbcMapper[AnyRef, AnyRef]

    def map(rs: ResultSet, i: Int): AnyRef

    def convertToJdbc(v: AnyRef): AnyRef
  } = new {
    val fa = fa0.asInstanceOf[JdbcMapper[AnyRef, AnyRef]]

    def map(rs: ResultSet, i: Int) = fa.map(rs, i)

    def convertToJdbc(v: AnyRef): AnyRef = {
      if (v != null)
        fa.convertToJdbc(v)
      else null
    }
  }

  protected class FieldAttributesBasedOnType[A](val mapper: MapperForReflection, val defaultLength: Int, val sample: A, val nativeJdbcType: Class[_]) {

    val clasz: Class[_] = sample.asInstanceOf[AnyRef].getClass

    override def toString: String =
      clasz.getCanonicalName + " --> " + mapper.getClass.getCanonicalName
  }

  def nativeJdbcValueFor(nonNativeType: Class[_], r: AnyRef): AnyRef =
    get(nonNativeType).mapper.convertToJdbc(r)

  def isSupported(c: Class[_]): Boolean =
    lookup(c).isDefined ||
      c.isAssignableFrom(classOf[Some[_]]) ||
      classOf[Product1[Any]].isAssignableFrom(c)

  def defaultColumnLength(c: Class[_]): Int =
    get(c).defaultLength

  def nativeJdbcTypeFor(c: Class[_]): Class[_] =
    get(c).nativeJdbcType

  def resultSetHandlerFor(c: Class[_]): (ResultSet, Int) => AnyRef = {
    val fa = get(c)
    (rs: ResultSet, i: Int) => {
      val z = fa.mapper.map(rs, i)
      if (rs.wasNull) null
      else z.asInstanceOf[AnyRef]
    }
  }

  private def get(c: Class[_]) =
    lookup(c).
      getOrElse(
        Utils.throwError("Usupported native type " + c.getCanonicalName + "," + c.getName + "\n" + registry.mkString("\n")))

  def sampleValueFor(c: Class[_]): AnyRef =
    get(c).sample.asInstanceOf[AnyRef]

  def trySampleValueFor(c: Class[_]): AnyRef = {
    val r = lookup(c).map(_.sample)
    r match {
      case Some(x: AnyRef) => x
      case _ => null
    }
  }

  private[squeryl] def register[P, A](m: NonPrimitiveJdbcMapper[P, A, _]): Unit = {

    val z = new FieldAttributesBasedOnType(
      makeMapper(m),
      m.defaultColumnLength,
      m.sample,
      m.primitiveMapper.nativeJdbcType)

    val wasThere = registry.put(z.clasz, z)

    if (wasThere.isDefined)
      Utils.throwError("field type " + z.clasz + " already registered, handled by " + m.getClass.getCanonicalName)
  }

  private[squeryl] def register[S, J](m: ArrayJdbcMapper[S, J]): Unit = {
    val f = m.thisTypedExpressionFactory
    val z = new FieldAttributesBasedOnType(
      makeMapper(m),
      m.defaultColumnLength,
      f.sample,
      m.nativeJdbcType)

    val wasThere = registry.put(z.clasz, z)

    if (wasThere.isDefined)
      Utils.throwError("field type " + z.clasz + " already registered, handled by " + m.getClass.getCanonicalName)
  }

  private def register[A](pm: PrimitiveJdbcMapper[A]): Unit = {
    val f = pm.thisTypedExpressionFactory
    val z = new FieldAttributesBasedOnType(
      makeMapper(pm),
      f.defaultColumnLength, f.sample, pm.nativeJdbcType)

    val c = z.clasz

    registry.put(c, z)
  }

  @tailrec
  private def lookup(c: Class[_]): Option[FieldAttributesBasedOnType[_]] = {
    if (!c.isPrimitive)
      registry.get(c)
    else c.getName match {
      case "int" => lookup(classOf[java.lang.Integer])
      case "long" => lookup(classOf[java.lang.Long])
      case "float" => lookup(classOf[java.lang.Float])
      case "byte" => lookup(classOf[java.lang.Byte])
      case "boolean" => lookup(classOf[java.lang.Boolean])
      case "double" => lookup(classOf[java.lang.Double])
      case "void" => None
    }
  }
}
