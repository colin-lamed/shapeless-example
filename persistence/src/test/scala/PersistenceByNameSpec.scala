package persistence

import org.scalatest.FlatSpec
import shapeless.{HNil, Witness, ::}
import shapeless.labelled.FieldType
import shapeless.syntax.singleton._ // for `->>` syntax
import shapeless.tag.@@


class PersistenceByNameSpec extends FlatSpec {
  import PersistenceByName._

  val conn = ConnectionHolder.conn

  def getFieldName[K <: String, V](value: FieldType[Symbol @@ K, V])(implicit witness: Witness.Aux[K]): K =
    witness.value

  def getFieldValue[K <: String, V](value: FieldType[Symbol @@ K, V]): V =
    value


  case class MyProcedureInput(
      one : String
    , two : String
    , res1: Param.Out[String]
    , res2: Param.Out[Boolean]
    )

  val params = MyProcedureInput(
      one  = "one"
    , two  = "two"
    , res1 = Param.Out.varchar
    , res2 = Param.Out.boolean
    )

  case class MyProcedureRes(
      res1: String
    , res2: Boolean
    )

  "Persistence3" should "call procedure with labelled params" in {

    val params = 'one  ->> Param.In("one")   ::
                 'two  ->> Param.In("two")   ::
                 'res1 ->> Param.Out.varchar ::
                 'res2 ->> Param.Out.boolean ::
                 HNil
    val strVal :: boolVal :: HNil = call(conn, "my_procedure", params)

    assert(getFieldName(strVal) === "res1")
    assert(getFieldValue(strVal) === "onetwo")
    assert(strVal === "onetwo")

    assert(getFieldName(boolVal) === "res2")
    assert(getFieldValue(boolVal) === false)
    assert(boolVal === false)

    // or with record syntax

    val res = call(conn, "my_procedure", params)

    import shapeless.record._

    assert(res('res1) === "onetwo")
    assert(res('res2) === false)
  }

  it should "call procedure with labelled params without Param.In" in {

    val params = 'one  ->> "one"   ::
                 'two  ->> "two"   ::
                 'res1 ->> Param.Out.varchar ::
                 'res2 ->> Param.Out.boolean ::
                 HNil
    val strVal :: boolVal :: HNil = call(conn, "my_procedure", params)

    assert(getFieldName(strVal) === "res1")
    assert(getFieldValue(strVal) === "onetwo")
    assert(strVal === "onetwo")

    assert(getFieldName(boolVal) === "res2")
    assert(getFieldValue(boolVal) === false)
    assert(boolVal === false)

    // or with record syntax

    val res = call(conn, "my_procedure", params)

    import shapeless.record._

    assert(res('res1) === "onetwo")
    assert(res('res2) === false)
  }

  it should "call procedure and return case class" in {
    val res = callAdt[MyProcedureRes](conn, "my_procedure", params)

    assert(res.res1 === "onetwo")
    assert(res.res2 === false)
  }
}