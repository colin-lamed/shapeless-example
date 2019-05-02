package persistence

import org.scalatest.FlatSpec
import shapeless.{HNil, ::}


class PersistenceByIndexSpec extends FlatSpec {
  import PersistenceByIndex._

  val conn = ConnectionHolder.conn

  "PersistenceByIndex" should "call procedure with typesafe params" in {
    val strVal :: boolVal :: HNil = call(conn, "my_procedure", Param.In("one") :: Param.In("two") :: Param.Out.varchar :: Param.Out.boolean :: HNil)
    assert(strVal === "onetwo")
    assert(boolVal === false)
  }

  it should "call procedure without Param.In wrapper" in {
    val strVal :: boolVal :: HNil = call(conn, "my_procedure", "one" :: "two" :: Param.Out.varchar :: Param.Out.boolean :: HNil)
    assert(strVal === "onetwo")
    assert(boolVal === false)
  }
}