package org.squeryl

import java.sql.ResultSet

package object dsl {

  type TEF[A, TA] = TypedExpressionFactory[A, TA] with PrimitiveJdbcMapper[A]

  object TEF {
    def apply[A, TA](theSample: A, columnLength: Int, extractor: ResultSet => Int => A): TEF[A, TA] =
      new TypedExpressionFactory[A, TA] with PrimitiveJdbcMapper[A] {
        override def sample = theSample

        override def defaultColumnLength = columnLength

        def extractNativeJdbcValue(rs: ResultSet, i: Int): A = extractor(rs)(i)
      }
  }

}