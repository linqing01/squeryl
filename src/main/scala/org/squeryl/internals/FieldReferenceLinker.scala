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

import net.sf.cglib.proxy._
import org.squeryl._
import org.squeryl.dsl.{CompositeKey, TypedExpression}
import org.squeryl.dsl.ast._

import java.lang.reflect.{Field, Method}
import scala.collection.mutable.ArrayBuffer

object FieldReferenceLinker {

  def clearThreadLocalState(): Unit = {
    _yieldValues.remove()
    __lastAccessedFieldReference.remove()
    _compositeKeyMembers.remove()
    _yieldInspectionTL.remove()
  }

  def pushExpressionOrCollectValue[T](e: () => TypedExpression[T, _]): T = {
    if (isYieldInspectionMode) {
      val yi = _yieldInspectionTL.get
      val expr = yi.callWithoutReentrance(e)
      yi.addSelectElement(new ValueSelectElement(expr, yi.resultSetMapper, expr.mapper, yi.queryExpressionNode))
      val r = expr.sample
      r
    }
    else {
      val r = _yieldValues.get.remove(0).asInstanceOf[T]
      if (_yieldValues.get.isEmpty) {
        _yieldValues.remove()
      }
      r
    }
  }

  def pushYieldValue(v: AnyRef): ArrayBuffer[AnyRef] = {
    var a = _yieldValues.get
    if (a == null) {
      a = new ArrayBuffer[AnyRef]
      _yieldValues.set(a)
    }
    a += v
  }

  def isYieldInspectionMode: Boolean = {
    val yi = _yieldInspectionTL.get
    if (yi != null) {
      yi.isOn
    } else {
      _yieldInspectionTL.remove()
      false
    }
  }

  def inspectedQueryExpressionNode: QueryExpressionNode[_] = _yieldInspectionTL.get.queryExpressionNode

  private[this] val _yieldValues = new ThreadLocal[ArrayBuffer[AnyRef]]

  private[this] val __lastAccessedFieldReference = new ThreadLocal[Option[SelectElement]]

  private[squeryl] def _lastAccessedFieldReference: Option[SelectElement] = {
    val fr = __lastAccessedFieldReference.get
    if (fr == null) None else fr
  }

  private[squeryl] def _lastAccessedFieldReference_=(se: Option[SelectElement]): Unit =
    if (se.isEmpty) {
      __lastAccessedFieldReference.remove()
    } else {
      __lastAccessedFieldReference.set(se)
    }

  private[this] val _compositeKeyMembers = new ThreadLocal[Option[ArrayBuffer[SelectElement]]]

  /**
   * _lastAccessedFieldReference is unique per thread, AST construction can be nested and can interfere with
   * one another, this method is used for  preserving the previous _lastAccessedFieldReference when a nested
   * AST construction takes place *and* during the construction of 'sample' POSOs, because they are proxied,
   * and can call their intercepted fields during construction, calling the constructor for 'sample' POSO construction
   * without wrapping with this methor would have the effect of 'polluting' the _lastAccessedFieldReference (issue 68). 
   */
  def executeAndRestoreLastAccessedFieldReference[A](expressionWithSideEffectsASTConstructionThreadLocalState: => A): A = {
    // if we are currently building an AST, we must save the (last) _lastAccessedFieldReference
    val prev = FieldReferenceLinker._lastAccessedFieldReference
    val a = expressionWithSideEffectsASTConstructionThreadLocalState
    // and restore it to the previous state (issue19)
    FieldReferenceLinker._lastAccessedFieldReference = prev
    a
  }

  class YieldInspection {

    private[this] val _utilizedFields = new ArrayBuffer[SelectElement]
    var _on = false
    var queryExpressionNode: QueryExpressionNode[_] = _
    var _resultSetMapper: ResultSetMapper = _

    def isOn: Boolean = _on

    def callWithoutReentrance[U](f: () => U): U = {
      val prev = _on
      _on = false
      val res = f()
      _on = prev
      res
    }

    def addSelectElement(e: SelectElement): Unit =
      if (!e.inhibited) {
        _utilizedFields.append(e)
        e.prepareColumnMapper(_utilizedFields.size)
      }

    def resultSetMapper: ResultSetMapper = _resultSetMapper

    private[this] var _reentranceDepth = 0

    def reentranceDepth: Int = _reentranceDepth

    def incrementReentranceDepth(): Unit =
      _reentranceDepth += 1

    def decrementReentranceDepth(): Unit =
      _reentranceDepth -= 1

    def turnOn(q: QueryExpressionNode[_], rsm: ResultSetMapper): Unit = {
      _reentranceDepth = 0
      queryExpressionNode = q
      _on = true
      _resultSetMapper = rsm
    }

    def outExpressions: List[SelectElement] = {
      _utilizedFields.toList
    }
  }

  private[this] val _yieldInspectionTL = new ThreadLocal[YieldInspection]

  def putLastAccessedSelectElement(e: SelectElement): Unit = {
    if (isYieldInspectionMode) {
      _yieldInspectionTL.get.addSelectElement(new ExportedSelectElement(e))
    } else
      _lastAccessedFieldReference = Some(e)
  }

  def takeLastAccessedFieldReference: Option[SelectElement] = {
    val res = _lastAccessedFieldReference
    _lastAccessedFieldReference = None
    res
  }

  private def _takeLastAccessedUntypedFieldReference: SelectElementReference[_, _] =
    FieldReferenceLinker.takeLastAccessedFieldReference match {
      case Some(n: SelectElement) => new SelectElementReference(n, NoOpOutMapper)
      case None => org.squeryl.internals.Utils.throwError("Thread local does not have a last accessed field... this is a severe bug !")
    }

  def createEqualityExpressionWithLastAccessedFieldReferenceAndConstant(e: Any, c: Any, l: CanLookup): LogicalBoolean = {

    l match {
      case CompositeKeyLookup =>
        e.asInstanceOf[CompositeKey].buildEquality(c.asInstanceOf[CompositeKey])
      case s: SimpleKeyLookup[_] =>
        createEqualityExpressionWithLastAccessedFieldReferenceAndConstant(c, Some(s))
      case UnknownCanLookup =>
        createEqualityExpressionWithLastAccessedFieldReferenceAndConstant(c, None)
    }
  }

  def createEqualityExpressionWithLastAccessedFieldReferenceAndConstant(c: Any, lOpt: Option[SimpleKeyLookup[_]]): LogicalBoolean = {
    val left = _takeLastAccessedUntypedFieldReference
    val right = lOpt map (l => l.convert.asInstanceOf[Any => TypedExpression[_, _]](c)) getOrElse new InputOnlyConstantExpressionNode(c)

    new BinaryOperatorNodeLogicalBoolean(left, right, "=")
  }

  /**
   * It is assumed that yield invocation for inspection will never be nested, since
   * a query is completely built (and it's yield inspection is done) before it can
   * be nested, this is unlikely to change, but documenting this assumption was
   * deemed useful, because this method would stop working (without complaining)
   * if (the assumption) was broken.   
   */

  def determineColumnsUtilizedInYeldInvocation(q: QueryExpressionNode[_], rsm: ResultSetMapper, selectClosure: () => AnyRef): (List[SelectElement], AnyRef) = {

    val yi = new YieldInspection
    _yieldInspectionTL.set(yi)
    var result: (List[SelectElement], AnyRef) = null
    try {
      yi.turnOn(q, rsm)

      val prev = _lastAccessedFieldReference
      val res0 =
        try {
          selectClosure()
        }
        finally {
          _lastAccessedFieldReference = prev
        }

      if (res0 == null)
        org.squeryl.internals.Utils.throwError("query " + q + " yielded null")

      val visitedSet = new java.util.IdentityHashMap[AnyRef, AnyRef]

      _populateSelectColsRecurse(visitedSet, yi, q, res0)

      result = (yi.outExpressions, res0)
    }
    finally {
      _yieldInspectionTL.remove()
    }
    result
  }

  /*
   * The theory here is that setting the var is an atomic operation, so
   * it should be ok to perform without synchronization.
   * Worst case scenario would be that one thread overwrites the map
   * created by a previous thread, but since it's just a cache and the fields
   * can be retrieved at any time, that's ok.
   */
  private object _declaredFieldCache {

    @volatile var _cache: Map[Class[_], Array[Field]] =
      Map[Class[_], Array[Field]]()

    def apply(cls: Class[_]) =
      _cache.getOrElse(cls, {
        val declaredFields = cls.getDeclaredFields()
        _cache += ((cls, declaredFields))
        declaredFields
      })

  }

  private def _populateSelectColsRecurse(visited: java.util.IdentityHashMap[AnyRef, AnyRef], yi: YieldInspection, q: QueryExpressionElements, o: AnyRef): Unit =
    if (o != null && o != None) {
      if (!visited.containsKey(o)) {
        val clazz = o.getClass
        val clazzName = clazz.getName
        //println("Looking at " + clazzName)
        if (!clazzName.startsWith("java.") &&
          !clazzName.startsWith("net.sf.cglib.") &&
          !clazzName.startsWith("scala.Enumeration")) {
          visited.put(o, o)
          _populateSelectCols(yi, q, o)
          for (f <- _declaredFieldCache(clazz)) {
            f.setAccessible(true)
            val ob = f.get(o)
            if (!f.getName.startsWith("CGLIB$") &&
              !f.getType.getName.startsWith("scala.Function") &&
              !FieldMetaData.factory.hideFromYieldInspection(o, f)) {
              _populateSelectColsRecurse(visited, yi, q, ob)
            }
          }
        }
      }
    }


  private def _populateSelectCols(yi: YieldInspection, q: QueryExpressionElements, sample: AnyRef): Unit = {
    val owner = _findQENThatOwns(sample, q)
    owner foreach { o =>
      for (e <- o.getOrCreateAllSelectElements(q))
        yi.addSelectElement(e)
    }
  }

  def findOwnerOfSample(s: AnyRef): Option[QueryableExpressionNode] =
  // TODO: could we enforce that Query[AnyVal] are not nested in some other way ?
  //    if(s.isInstanceOf[AnyVal])
  //      org.squeryl.internals.Utils.throwError("A query that returns a AnyVal cannot be nested " + Utils.failSafeString(FieldReferenceLinker.inspectedQueryExpressionNode.toString))
  //    else
    _findQENThatOwns(s, FieldReferenceLinker.inspectedQueryExpressionNode)

  private def _findQENThatOwns(sample: AnyRef, q: QueryExpressionElements): Option[QueryableExpressionNode] = {

    q.filterDescendantsOfType[QueryableExpressionNode].find(_.owns(sample))
  }

  def createCallBack(v: ViewExpressionNode[_]): Callback =
    new PosoPropertyAccessInterceptor(v)

  private class PosoPropertyAccessInterceptor(val viewExpressionNode: ViewExpressionNode[_]) extends MethodInterceptor {

    def fmd4Method(m: Method) =
      viewExpressionNode.view.findFieldMetaDataForProperty(m.getName)

    def intercept(o: Object, m: Method, args: Array[Object], proxy: MethodProxy): Object = {

      val fmd = fmd4Method(m)
      val yi = if (isYieldInspectionMode) _yieldInspectionTL.get else null
      val isComposite =
        classOf[CompositeKey].isAssignableFrom(m.getReturnType)

      try {
        if (fmd.isDefined && yi != null)
          yi.incrementReentranceDepth()

        _intercept(o, m, args, proxy, fmd, yi, isComposite)
      }
      finally {
        if (fmd.isDefined && yi != null)
          yi.decrementReentranceDepth()
      }
    }

    private def _intercept(o: Object, m: Method, args: Array[Object], proxy: MethodProxy, fmd: Option[FieldMetaData], yi: YieldInspection, isComposite: Boolean): Object = {

      if (isComposite)
        _compositeKeyMembers.set(Some(new ArrayBuffer[SelectElement]))

      val res =
        if (m.getName.equals("toString") && m.getParameterTypes.length == 0)
          "sample:" + viewExpressionNode.view.name + "[" + Integer.toHexString(System.identityHashCode(o)) + "]"
        else
          proxy.invokeSuper(o, args)

      if (isComposite) {
        val ck = res.asInstanceOf[CompositeKey]
        ck._members = Some(_compositeKeyMembers.get.get.map(new SelectElementReference[Any, Any](_, NoOpOutMapper)))
        ck._propertyName = Some(m.getName)
        _compositeKeyMembers.remove()
      }

      if (fmd.isDefined) {

        if (yi != null && yi.reentranceDepth == 1)
          yi.addSelectElement(viewExpressionNode.getOrCreateSelectElement(fmd.get, yi.queryExpressionNode))

        if (_compositeKeyMembers.get == null) {
          _compositeKeyMembers.remove()
          _lastAccessedFieldReference = Some(viewExpressionNode.getOrCreateSelectElement(fmd.get))
        } else
          _compositeKeyMembers.get.get.append(viewExpressionNode.getOrCreateSelectElement(fmd.get))
      }

      res
    }
  }

}
