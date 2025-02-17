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
package org.squeryl.dsl

import org.squeryl.dsl.ast._
import org.squeryl.internals.{AttributeValidOnMultipleColumn, FieldMetaData}

trait CompositeKey {

  private[squeryl] var _members: Option[collection.Seq[SelectElementReference[_, _]]] = None

  private[squeryl] var _propertyName: Option[String] = None

  private[squeryl] def _fields: collection.Seq[FieldMetaData] =
    if (_members.isEmpty)
      List.empty
    else
      _members.get.map(_.selectElement.asInstanceOf[FieldSelectElement].fieldMetaData)

  protected def constantMembers: Iterable[TypedExpression[_, _]]

  protected def members: Iterable[TypedExpression[_, _]] =
    _members.getOrElse(constantMembers)

  private[squeryl] def buildEquality(ck: CompositeKey): LogicalBoolean = {
    val equalities = (members zip ck.members).map(t => new EqualityExpression(t._1, t._2))

    equalities.head
    val tail = equalities.tail

    tail.foldLeft(equalities.head: LogicalBoolean)(_ and _)
  }

  def is(attributes: AttributeValidOnMultipleColumn*) = new CompositeKeyAttributeAssignment(this, attributes)

  protected def inExpr(cks: Iterable[CompositeKey]): LogicalBoolean = {
    new InclusionOperator(
      new RowValueConstructorNode(members.toList),
      new RightHandSideOfIn(
        new ListExpressionNode(
          cks.toList map (ck =>
            new RowValueConstructorNode(ck.members.toList)
            )
        ),
        Some(true)
      )
    )
  }

  protected def notInExpr(cks: Iterable[CompositeKey]): LogicalBoolean = {
    new ExclusionOperator(
      new RowValueConstructorNode(members.toList),
      new RightHandSideOfIn(
        new ListExpressionNode(
          cks.toList map (ck =>
            new RowValueConstructorNode(ck.members.toList)
            )
        ),
        Some(false)
      )
    )
  }
}

case class CompositeKey2[A1, A2](a1: A1, a2: A2)(
  implicit
  ev1: A1 => TypedExpression[A1, _],
  ev2: A2 => TypedExpression[A2, _])
  extends CompositeKey {

  def ===(ck: CompositeKey2[A1, A2]): LogicalBoolean =
    buildEquality(ck)

  def ===(ck: (A1, A2)): LogicalBoolean =
    buildEquality(CompositeKey2(ck._1, ck._2))

  def in(cks: CompositeKey2[A1, A2]*): LogicalBoolean = inExpr(cks)

  def inTuples(cks: (A1, A2)*): LogicalBoolean = inExpr(cks map (ck => CompositeKey2(ck._1, ck._2)))

  def notIn(cks: CompositeKey2[A1, A2]*): LogicalBoolean = notInExpr(cks)

  def notInTuples(cks: (A1, A2)*): LogicalBoolean = notInExpr(cks map (ck => CompositeKey2(ck._1, ck._2)))

  protected def constantMembers: Iterable[TypedExpression[_, _]] = List(a1, a2)

}

case class CompositeKey3[A1, A2, A3](a1: A1, a2: A2, a3: A3)(
  implicit
  ev1: A1 => TypedExpression[A1, _],
  ev2: A2 => TypedExpression[A2, _],
  ev3: A3 => TypedExpression[A3, _])
  extends CompositeKey {

  def ===(ck: CompositeKey3[A1, A2, A3]): LogicalBoolean =
    buildEquality(ck)

  def ===(ck: (A1, A2, A3)): LogicalBoolean =
    buildEquality(CompositeKey3(ck._1, ck._2, ck._3))

  def in(cks: CompositeKey3[A1, A2, A3]*): LogicalBoolean = inExpr(cks)

  def inTuples(cks: (A1, A2, A3)*): LogicalBoolean = inExpr(cks map (ck => CompositeKey3(ck._1, ck._2, ck._3)))

  def notIn(cks: CompositeKey3[A1, A2, A3]*): LogicalBoolean = notInExpr(cks)

  def notInTuples(cks: (A1, A2, A3)*): LogicalBoolean = notInExpr(cks map (ck => CompositeKey3(ck._1, ck._2, ck._3)))

  protected def constantMembers: Iterable[TypedExpression[_, _]] = List(a1, a2, a3)
}

case class CompositeKey4[A1, A2, A3, A4](a1: A1, a2: A2, a3: A3, a4: A4)(
  implicit
  ev1: A1 => TypedExpression[A1, _],
  ev2: A2 => TypedExpression[A2, _],
  ev3: A3 => TypedExpression[A3, _],
  ev4: A4 => TypedExpression[A4, _])
  extends CompositeKey {

  def ===(ck: CompositeKey4[A1, A2, A3, A4]): LogicalBoolean =
    buildEquality(ck)

  def ===(ck: (A1, A2, A3, A4)): LogicalBoolean =
    buildEquality(CompositeKey4(ck._1, ck._2, ck._3, ck._4))

  def in(cks: CompositeKey4[A1, A2, A3, A4]*): LogicalBoolean = inExpr(cks)

  def inTuples(cks: (A1, A2, A3, A4)*): LogicalBoolean = inExpr(cks map (ck => CompositeKey4(ck._1, ck._2, ck._3, ck._4)))

  def notIn(cks: CompositeKey4[A1, A2, A3, A4]*): LogicalBoolean = notInExpr(cks)

  def notInTuples(cks: (A1, A2, A3, A4)*): LogicalBoolean = notInExpr(cks map (ck => CompositeKey4(ck._1, ck._2, ck._3, ck._4)))

  protected def constantMembers: Iterable[TypedExpression[_, _]] = List(a1, a2, a3, a4)
}


case class CompositeKey5[A1, A2, A3, A4, A5](a1: A1, a2: A2, a3: A3, a4: A4, a5: A5)(
  implicit
  ev1: A1 => TypedExpression[A1, _],
  ev2: A2 => TypedExpression[A2, _],
  ev3: A3 => TypedExpression[A3, _],
  ev4: A4 => TypedExpression[A4, _],
  ev5: A5 => TypedExpression[A5, _])
  extends CompositeKey {

  def ===(ck: CompositeKey5[A1, A2, A3, A4, A5]): LogicalBoolean =
    buildEquality(ck)

  def ===(ck: (A1, A2, A3, A4, A5)): LogicalBoolean =
    buildEquality(CompositeKey5(ck._1, ck._2, ck._3, ck._4, ck._5))

  def in(cks: CompositeKey5[A1, A2, A3, A4, A5]*): LogicalBoolean = inExpr(cks)

  def inTuples(cks: (A1, A2, A3, A4, A5)*): LogicalBoolean = inExpr(cks map (ck => CompositeKey5(ck._1, ck._2, ck._3, ck._4, ck._5)))

  def notIn(cks: CompositeKey5[A1, A2, A3, A4, A5]*): LogicalBoolean = notInExpr(cks)

  def notInTuples(cks: (A1, A2, A3, A4, A5)*): LogicalBoolean = notInExpr(cks map (ck => CompositeKey5(ck._1, ck._2, ck._3, ck._4, ck._5)))

  protected def constantMembers: Iterable[TypedExpression[_, _]] = List(a1, a2, a3, a4, a5)
}

case class CompositeKey6[A1, A2, A3, A4, A5, A6](a1: A1, a2: A2, a3: A3, a4: A4, a5: A5, a6: A6)(
  implicit
  ev1: A1 => TypedExpression[A1, _],
  ev2: A2 => TypedExpression[A2, _],
  ev3: A3 => TypedExpression[A3, _],
  ev4: A4 => TypedExpression[A4, _],
  ev5: A5 => TypedExpression[A5, _],
  ev6: A6 => TypedExpression[A6, _])
  extends CompositeKey {

  def ===(ck: CompositeKey6[A1, A2, A3, A4, A5, A6]): LogicalBoolean =
    buildEquality(ck)

  def ===(ck: (A1, A2, A3, A4, A5, A6)): LogicalBoolean =
    buildEquality(CompositeKey6(ck._1, ck._2, ck._3, ck._4, ck._5, ck._6))

  def in(cks: CompositeKey6[A1, A2, A3, A4, A5, A6]*): LogicalBoolean = inExpr(cks)

  def inTuples(cks: (A1, A2, A3, A4, A5, A6)*): LogicalBoolean = inExpr(cks map (ck => CompositeKey6(ck._1, ck._2, ck._3, ck._4, ck._5, ck._6)))

  def notIn(cks: CompositeKey6[A1, A2, A3, A4, A5, A6]*): LogicalBoolean = notInExpr(cks)

  def notInTuples(cks: (A1, A2, A3, A4, A5, A6)*): LogicalBoolean = notInExpr(cks map (ck => CompositeKey6(ck._1, ck._2, ck._3, ck._4, ck._5, ck._6)))

  protected def constantMembers: Iterable[TypedExpression[_, _]] = List(a1, a2, a3, a4, a5, a6)
}

case class CompositeKey7[A1, A2, A3, A4, A5, A6, A7](a1: A1, a2: A2, a3: A3, a4: A4, a5: A5, a6: A6, a7: A7)(
  implicit
  ev1: A1 => TypedExpression[A1, _],
  ev2: A2 => TypedExpression[A2, _],
  ev3: A3 => TypedExpression[A3, _],
  ev4: A4 => TypedExpression[A4, _],
  ev5: A5 => TypedExpression[A5, _],
  ev6: A6 => TypedExpression[A6, _],
  ev7: A7 => TypedExpression[A7, _])
  extends CompositeKey {

  def ===(ck: CompositeKey7[A1, A2, A3, A4, A5, A6, A7]): LogicalBoolean =
    buildEquality(ck)

  def ===(ck: (A1, A2, A3, A4, A5, A6, A7)): LogicalBoolean =
    buildEquality(CompositeKey7(ck._1, ck._2, ck._3, ck._4, ck._5, ck._6, ck._7))

  def in(cks: CompositeKey7[A1, A2, A3, A4, A5, A6, A7]*): LogicalBoolean = inExpr(cks)

  def inTuples(cks: (A1, A2, A3, A4, A5, A6, A7)*): LogicalBoolean = inExpr(cks map (ck => CompositeKey7(ck._1, ck._2, ck._3, ck._4, ck._5, ck._6, ck._7)))

  def notIn(cks: CompositeKey7[A1, A2, A3, A4, A5, A6, A7]*): LogicalBoolean = notInExpr(cks)

  def notInTuples(cks: (A1, A2, A3, A4, A5, A6, A7)*): LogicalBoolean = notInExpr(cks map (ck => CompositeKey7(ck._1, ck._2, ck._3, ck._4, ck._5, ck._6, ck._7)))

  protected def constantMembers: Iterable[TypedExpression[_, _]] = List(a1, a2, a3, a4, a5, a6, a7)
}

case class CompositeKey8[A1, A2, A3, A4, A5, A6, A7, A8](a1: A1, a2: A2, a3: A3, a4: A4, a5: A5, a6: A6, a7: A7, a8: A8)(
  implicit
  ev1: A1 => TypedExpression[A1, _],
  ev2: A2 => TypedExpression[A2, _],
  ev3: A3 => TypedExpression[A3, _],
  ev4: A4 => TypedExpression[A4, _],
  ev5: A5 => TypedExpression[A5, _],
  ev6: A6 => TypedExpression[A6, _],
  ev7: A7 => TypedExpression[A7, _],
  ev8: A8 => TypedExpression[A8, _])
  extends CompositeKey {

  def ===(ck: CompositeKey8[A1, A2, A3, A4, A5, A6, A7, A8]): LogicalBoolean =
    buildEquality(ck)

  def ===(ck: (A1, A2, A3, A4, A5, A6, A7, A8)): LogicalBoolean =
    buildEquality(CompositeKey8(ck._1, ck._2, ck._3, ck._4, ck._5, ck._6, ck._7, ck._8))

  def in(cks: CompositeKey8[A1, A2, A3, A4, A5, A6, A7, A8]*): LogicalBoolean = inExpr(cks)

  def inTuples(cks: (A1, A2, A3, A4, A5, A6, A7, A8)*): LogicalBoolean = inExpr(cks map (ck => CompositeKey8(ck._1, ck._2, ck._3, ck._4, ck._5, ck._6, ck._7, ck._8)))

  def notIn(cks: CompositeKey8[A1, A2, A3, A4, A5, A6, A7, A8]*): LogicalBoolean = notInExpr(cks)

  def notInTuples(cks: (A1, A2, A3, A4, A5, A6, A7, A8)*): LogicalBoolean = notInExpr(cks map (ck => CompositeKey8(ck._1, ck._2, ck._3, ck._4, ck._5, ck._6, ck._7, ck._8)))

  protected def constantMembers: Iterable[TypedExpression[_, _]] = List(a1, a2, a3, a4, a5, a6, a7, a8)
}

case class CompositeKey9[A1, A2, A3, A4, A5, A6, A7, A8, A9](a1: A1, a2: A2, a3: A3, a4: A4, a5: A5, a6: A6, a7: A7, a8: A8, a9: A9)(
  implicit
  ev1: A1 => TypedExpression[A1, _],
  ev2: A2 => TypedExpression[A2, _],
  ev3: A3 => TypedExpression[A3, _],
  ev4: A4 => TypedExpression[A4, _],
  ev5: A5 => TypedExpression[A5, _],
  ev6: A6 => TypedExpression[A6, _],
  ev7: A7 => TypedExpression[A7, _],
  ev8: A8 => TypedExpression[A8, _],
  ev9: A9 => TypedExpression[A9, _])
  extends CompositeKey {

  def ===(ck: CompositeKey9[A1, A2, A3, A4, A5, A6, A7, A8, A9]): LogicalBoolean =
    buildEquality(ck)

  def ===(ck: (A1, A2, A3, A4, A5, A6, A7, A8, A9)): LogicalBoolean =
    buildEquality(CompositeKey9(ck._1, ck._2, ck._3, ck._4, ck._5, ck._6, ck._7, ck._8, ck._9))

  def in(cks: CompositeKey9[A1, A2, A3, A4, A5, A6, A7, A8, A9]*): LogicalBoolean = inExpr(cks)

  def inTuples(cks: (A1, A2, A3, A4, A5, A6, A7, A8, A9)*): LogicalBoolean = inExpr(cks map (ck => CompositeKey9(ck._1, ck._2, ck._3, ck._4, ck._5, ck._6, ck._7, ck._8, ck._9)))

  def notIn(cks: CompositeKey9[A1, A2, A3, A4, A5, A6, A7, A8, A9]*): LogicalBoolean = notInExpr(cks)

  def notInTuples(cks: (A1, A2, A3, A4, A5, A6, A7, A8, A9)*): LogicalBoolean = notInExpr(cks map (ck => CompositeKey9(ck._1, ck._2, ck._3, ck._4, ck._5, ck._6, ck._7, ck._8, ck._9)))

  protected def constantMembers: Iterable[TypedExpression[_, _]] = List(a1, a2, a3, a4, a5, a6, a7, a8, a9)
}
