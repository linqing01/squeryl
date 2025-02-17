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

import org.squeryl.internals.{FieldMetaData, StatementWriter}

class UpdateStatement(_whereClause: Option[() => LogicalBoolean], uas: collection.Seq[UpdateAssignment])
  extends ExpressionNode {

  val whereClause: Option[LogicalBoolean] =
    _whereClause.map(_.apply())

  override def children: List[ExpressionNode] = whereClause.toList ++ values

  def doWrite(sw: StatementWriter): Unit = {}

  def columns: collection.Seq[FieldMetaData] =
    uas.map(ua => ua.left)

  def values: collection.Seq[ExpressionNode] =
    uas.map(ua => ua.right)
}
