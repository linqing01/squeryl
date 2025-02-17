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

class Group[K](k: K) {
  def key: K = k
}

class Measures[M](m: M) {
  def measures: M = m
}

class GroupWithMeasures[K, M](k: K, m: M) {
  def key: K = k

  def measures: M = m

  override def toString: String = {
    val sb = new java.lang.StringBuilder
    sb.append("GroupWithMeasures[")
    sb.append("key=")
    sb.append(key)
    sb.append(",measures=")
    sb.append(measures)
    sb.append("]")
    sb.toString
  }
}

object GroupWithMeasures {
  def unapply[K, M](x: GroupWithMeasures[K, M]): Some[(K, M)] = Some((x.key, x.measures))
}
