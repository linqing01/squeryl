package org.squeryl.test

import org.squeryl._
import org.squeryl.framework.{DBConnector, RunTestsInsideTransaction, SchemaTester}
import org.squeryl.test.PrimitiveTypeModeForTests._

import java.util.UUID

object SubQueryTestSchema {

  class Entity(
                val name: String) extends KeyedEntity[UUID] {
    val id: UUID = new UUID(0, 0)
  }

  class EntityToTypeJoins(
                           val entityId: UUID,
                           val entType: String) {}

  class EntityEdge(
                    val parentId: UUID,
                    val childId: UUID,
                    val relationship: String,
                    val distance: Int) extends KeyedEntity[Long] {
    var id: Long = 0

  }

  object TestSchema extends Schema {
    val entity = table[Entity]()
    val entityType = table[EntityToTypeJoins]()
    val entityEdges = table[EntityEdge]()

    override def drop(): Unit = {
      Session.cleanupResources()
      super.drop()
    }
  }

}

abstract class SubQueryTests extends SchemaTester with RunTestsInsideTransaction {
  self: DBConnector =>

  import SubQueryTestSchema._


  final def schema = TestSchema

  test("Missing internal state, cant copy") {
    import TestSchema._

    val name = "llll"
    val typeName = "mmmm"
    val relType = "owns"

    val nameQuery = from(entity)(e => where(e.name === name) select e)

    val nameQueryId = from(nameQuery)(i => select(i.id))
    val typeQuery = from(entityType)(eType => where(eType.entType === typeName) select eType.entityId)

    val entEdges =
      from(entity, entityEdges)((e, edge) =>
        where((e.id === edge.childId) and (edge.parentId in nameQueryId) and (e.id in typeQuery) and (edge.relationship === relType))
          select(e, edge)
      )

    from(entEdges)(ee => select(ee._1))
  }
}