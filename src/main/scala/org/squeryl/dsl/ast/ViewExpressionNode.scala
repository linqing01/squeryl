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
package org.squeryl.dsl.ast

import org.squeryl.internals.{FieldMetaData, ResultSetMapper, StatementWriter}
import org.squeryl.{Session, View}

import scala.collection.mutable

class ViewExpressionNode[U](val view: View[U])
  extends QueryableExpressionNode {

  private[this] val _selectElements = new mutable.HashMap[FieldMetaData, SelectElement]

  def isChild(q: QueryableExpressionNode) = false

  def getOrCreateAllSelectElements(forScope: QueryExpressionElements): Iterable[SelectElement] = {

    val export = !forScope.isChild(this)

    view.posoMetaData.fieldsMetaData.map(fmd =>
      getOrCreateSelectElement(fmd, export)
    )
  }

  private def getOrCreateSelectElement(fmd: FieldMetaData, export: Boolean): SelectElement = {

    val e = _selectElements.get(fmd)
    val n =
      if (e.isDefined)
        e.get
      else {
        val r = new FieldSelectElement(this, fmd, resultSetMapper)
        _selectElements.put(fmd, r)
        r
      }

    if (export)
      new ExportedSelectElement(n)
    else
      n
  }

  def getOrCreateSelectElement(fmd: FieldMetaData): SelectElement =
    getOrCreateSelectElement(fmd, export = false)

  def getOrCreateSelectElement(fmd: FieldMetaData, forScope: QueryExpressionElements): SelectElement =
    getOrCreateSelectElement(fmd, !forScope.isChild(this))


  val resultSetMapper = new ResultSetMapper

  def alias: String =
    Session.currentSession.databaseAdapter.viewAlias(this)

  def owns(aSample: AnyRef): Boolean = aSample eq sample.asInstanceOf[AnyRef]

  private[this] var _sample: Option[U] = None

  private[squeryl] def sample_=(d: U): Unit =
    _sample = Some(d)

  def sample: U = _sample.get

  def doWrite(sw: StatementWriter): Unit =
    sw.write(sw.quoteName(view.prefixedName))

  override def toString: String = {
    val sb = new java.lang.StringBuilder
    sb.append("'ViewExpressionNode[")
    sb.append(sample)
    sb.append("]:")
    sb.append("rsm=")
    sb.append(resultSetMapper)
    sb.toString
  }
}
