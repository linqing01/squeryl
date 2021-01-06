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
package org.squeryl.dsl.fsm

import org.squeryl.Query
import org.squeryl.dsl._
import org.squeryl.dsl.ast._
import org.squeryl.dsl.boilerplate._
import org.squeryl.internals.{ColumnToTupleMapper, FieldReferenceLinker, OutMapper, ResultSetMapper}

import java.sql.ResultSet

class BaseQueryYield[G]
(val queryElementzz: QueryElements[_], val selectClosure: () => G)
  extends SelectState[G]
    with OrderBySignatures[G]
    with QueryYield[G] {

  protected def _createColumnToTupleMapper(origin: QueryExpressionNode[_], agregateArgs: List[TypedExpression[_, _]], offsetInResultSet: Int, isForGroup: Boolean): (ColumnToTupleMapper, List[TupleSelectElement]) = {

    var i = -1
    val nodes = agregateArgs.map(e => {
      i += 1; new TupleSelectElement(origin, e, i, isForGroup)
    })

    var o = offsetInResultSet

    val mappers = new Array[OutMapper[_]](agregateArgs.size)

    var k: Int = 0
    agregateArgs.foreach(e => {
      e.mapper.index = o
      o += 1
      mappers(k) = e.mapper
      k += 1
    })

    val m = new ColumnToTupleMapper(mappers)

    for (n <- nodes)
      n.columnToTupleMapper = Some(m)
    (m, nodes)
  }

  protected var _havingClause: Option[() => LogicalBoolean] = None

  def unevaluatedHavingClause: Option[() => LogicalBoolean] = _havingClause

  //TODO: an array is probably more efficient, even if less 'lazy' :
  protected var _orderByExpressions: () => List[() => ExpressionNode] = _

  def whereClause: Option[ExpressionNode] =
    queryElementzz.whereClause.map(b => b())

  def havingClause: Option[ExpressionNode] =
    _havingClause.map(c => c())

  def groupByClause: Iterable[ExpressionNode] = Iterable.empty

  def commonTableExpressions: Iterable[Query[_]] = queryElementzz.commonTableExpressions

  def queryElements: (Option[O], Option[O], Iterable[O], Iterable[O], Iterable[Query[_]]) =
    (whereClause, havingClause, groupByClause, orderByClause, commonTableExpressions)

  def computeClause: List[ExpressionNode] = List.empty

  def orderByClause: Iterable[ExpressionNode] = {
    if (_orderByExpressions == null)
      List.empty
    else
      _orderByExpressions().map(c => c())
  }

  def invokeYield(rsm: ResultSetMapper, rs: ResultSet): G =
    selectClosure()

  def invokeYieldForAst(q: QueryExpressionNode[_], rsm: ResultSetMapper): (Iterable[SelectElement], AnyRef) =
    FieldReferenceLinker.determineColumnsUtilizedInYeldInvocation(
      q, rsm, () => invokeYield(rsm, null).asInstanceOf[AnyRef])


  protected def _sTuple1ToValue[B](b: B): B =
    b match {
      case t: STuple1[_] =>
        if (t.productArity == 1)
          t._1.asInstanceOf[B]
        else b
    }
}

class GroupQueryYield[K](
                          _qe: QueryElements[_],
                          val groupByClauseClosure: () => List[TypedExpression[_, _]]
                        )
  extends BaseQueryYield[Group[K]](_qe, null)
    with GroupByState[K]
    with HavingState[K]
    with OrderBySignatures[Group[K]]
    with QueryYield[Group[K]] {

  override def groupByClause: List[ExpressionNode] =
    groupByClauseClosure().map(e => e)

  override def invokeYield(rsm: ResultSetMapper, rs: ResultSet): Group[K] =
    new Group(rsm.groupKeysMapper.get.mapToTuple(rs))

  override def queryElements: (Option[O], Option[O], Iterable[O], Iterable[O], Iterable[Query[_]]) =
    (whereClause, havingClause, groupByClause, orderByClause, commonTableExpressions)

  class SampleGroup[K](k: K)
    extends Group(k) {

    override def key: K = _sTuple1ToValue(k)
  }

  override def invokeYieldForAst(q: QueryExpressionNode[_], rsm: ResultSetMapper): (List[SelectElement], AnyRef) = {
    val offset = 1
    val (m, nodes) = _createColumnToTupleMapper(q, groupByClauseClosure(), offset, isForGroup = true)
    rsm.groupKeysMapper = Some(m)
    val st = SampleTuple.create(nodes, m.outMappers).asInstanceOf[K]
    (nodes, new SampleGroup(st))
  }
}

class MeasuresQueryYield[M](
                             _qe: QueryElements[_],
                             _computeByClauseClosure: () => List[TypedExpression[_, _]]
                           )
  extends BaseQueryYield[Measures[M]](_qe, null)
    with OrderBySignatures[Measures[M]]
    with ComputeStateStartOrWhereState[M]
    with QueryYield[Measures[M]] {
  override def invokeYield(rsm: ResultSetMapper, rs: ResultSet): Measures[M] =
    new Measures(rsm.groupMeasuresMapper.get.mapToTuple(rs))

  override def queryElements: (Option[O], Option[O], Iterable[O], Iterable[O], Iterable[Query[_]]) =
    (whereClause, havingClause, groupByClause, orderByClause, commonTableExpressions)


  class SampleMeasures[M](m: M)
    extends Measures(m) {

    override def measures: M = _sTuple1ToValue(m)
  }

  override def invokeYieldForAst(q: QueryExpressionNode[_], rsm: ResultSetMapper): (List[SelectElement], AnyRef) = {
    val offset = 1
    val (m, nodes) = _createColumnToTupleMapper(q, _computeByClauseClosure(), offset, isForGroup = false)
    rsm.groupMeasuresMapper = Some(m)
    val st = SampleTuple.create(nodes, m.outMappers).asInstanceOf[M]
    (nodes, new SampleMeasures(st))
  }
}

class GroupWithMeasuresQueryYield[K, M](
                                         _qe: QueryElements[_],
                                         _groupByClauseClosure: () => List[TypedExpression[_, _]],
                                         _having: Option[() => LogicalBoolean],
                                         _computeClauseClosure: () => List[TypedExpression[_, _]]
                                       )
  extends BaseQueryYield[GroupWithMeasures[K, M]](_qe, null)
    with ComputeStateFromGroupByState[K, M]
    with OrderBySignatures[GroupWithMeasures[K, M]]
    with QueryYield[GroupWithMeasures[K, M]] {

  class SampleGroupWithMeasures[K, M](k: K, m: M)
    extends GroupWithMeasures(k, m) {

    override def key: K = _sTuple1ToValue(k)

    override def measures: M = _sTuple1ToValue(m)
  }

  override def havingClause: Option[O] =
    if (_having.isDefined)
      _having.map(c => c())
    else
      super.havingClause

  override def queryElements: (Option[O], Option[O], Iterable[O], Iterable[O], Iterable[Query[_]]) =
    (whereClause, havingClause, _groupByClauseClosure().map(e => e), orderByClause, commonTableExpressions)

  override def invokeYield(rsm: ResultSetMapper, rs: ResultSet) =
    new GroupWithMeasures(rsm.groupKeysMapper.get.mapToTuple(rs), rsm.groupMeasuresMapper.get.mapToTuple(rs))

  override def invokeYieldForAst(q: QueryExpressionNode[_], rsm: ResultSetMapper): (List[SelectElement], AnyRef) = {

    val offset = 1

    val (km, knodes) = _createColumnToTupleMapper(q, _groupByClauseClosure(), offset, isForGroup = true)
    val (mm, mnodes) = _createColumnToTupleMapper(q, _computeClauseClosure(), offset + knodes.size, isForGroup = false)

    rsm.groupKeysMapper = Some(km)
    rsm.groupMeasuresMapper = Some(mm)

    val stK = SampleTuple.create(knodes, km.outMappers).asInstanceOf[K]
    val stM = SampleTuple.create(mnodes, mm.outMappers).asInstanceOf[M]

    (List(knodes, mnodes).flatten, new SampleGroupWithMeasures(stK, stM))
  }
}
