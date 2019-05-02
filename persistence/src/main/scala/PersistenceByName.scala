package persistence

import java.sql.Connection
import shapeless.{HNil, HList, LabelledGeneric, Witness, ::}
import shapeless.labelled.{field, FieldType}
import shapeless.ops.record.ToMap
import shapeless.tag
import shapeless.tag.@@


/** Invoke db procedure with parameters identified by name.
  * The inputs/outputs can be provided by tagged HList, or case classes with matching property names.
  */
object PersistenceByName {

  sealed trait Param
  object Param {
    case class In(value: Any) extends Param
    /** @param typeName required for some sqlTypes, like Types.ARRAY */
    case class Out[A](sqlType: Int, typeName: String = "") extends Param

    object Out {
      val varchar =
        Param.Out[String](sqlType = java.sql.Types.VARCHAR)
      val boolean =
        Param.Out[Boolean](sqlType = java.sql.Types.BOOLEAN)
    }
  }

  //
  // ToParam converts tagged HList to a List of Param.
  // First look for Param.In or Param.Out adding into List.
  // Then assume any other values should be wrapped in Param.In before adding into List.
  //

  trait ToParam[L <: HList] {
    def apply(params : L): Map[String, Param]
  }

  trait LowerPrecedenceToParam {
    implicit def assumeInToParam[K <: String, A, Rest <: HList](implicit toParam: ToParam[Rest], witness: Witness.Aux[K]) =
      new ToParam[FieldType[Symbol @@ K, A] :: Rest] {
        def apply(params: FieldType[Symbol @@ K, A] :: Rest): Map[String, Param] =
          Map(witness.value -> Param.In(params.head)) ++ toParam.apply(params.tail)
      }
  }

  object ToParam extends LowerPrecedenceToParam {
    implicit val nilToParam = new ToParam[HNil] {
      def apply(params: HNil): Map[String, Param] = Map.empty
    }

    implicit def inToParam[K <: String, A, Rest <: HList](implicit toParam: ToParam[Rest], witness: Witness.Aux[K]) =
      new ToParam[FieldType[Symbol @@ K, Param.In] :: Rest] {
        def apply(params: FieldType[Symbol @@ K, Param.In] :: Rest): Map[String, Param] =
          Map(witness.value -> params.head) ++ toParam.apply(params.tail)
      }

    implicit def outToParam[K <: String, A, Rest <: HList](implicit toParam: ToParam[Rest], witness: Witness.Aux[K]) =
      new ToParam[FieldType[Symbol @@ K, Param.Out[A]] :: Rest] {
        def apply(params: FieldType[Symbol @@ K, Param.Out[A]] :: Rest): Map[String, Param] =
          Map(witness.value -> params.head) ++ toParam.apply(params.tail)
      }
  }

  //
  // FromParam converts List of results into tagged HList, getting the type from the initial HList.
  // First try Param.Out[A] first, adding the output value to HList.
  // Then assume any other values are inputs, and skip output.
  //

  trait FromParam[L <: HList] {
    type Out <: HList
    def apply(params : L, params2: Map[String, Object]): Out
  }

  trait LowerPrecedenceFromParam {
    implicit def inFromParam[K <: String, A, Rest <: HList](implicit fromParam: FromParam[Rest]) =
      new FromParam[FieldType[Symbol @@ K, A] :: Rest] {
        type Out = fromParam.Out
        def apply(params: FieldType[Symbol @@ K, A] :: Rest, params2: Map[String, Object]): Out =
          fromParam.apply(params.tail, params2)
      }
  }

  object FromParam extends LowerPrecedenceFromParam {
    type Aux[L <: HList, Out0] = FromParam[L] { type Out = Out0 }

    implicit val nilFromParam = new FromParam[HNil] {
      type Out = HNil
      def apply(params: HNil, params2: Map[String, Object]): HNil = HNil
    }

    implicit def outFromParam[K <: String, A, Rest <: HList](implicit fromParam: FromParam[Rest], witness: Witness.Aux[K]) =
      new FromParam[FieldType[Symbol @@ K, Param.Out[A]] :: Rest] {
        type Out = FieldType[Symbol @@ K, A] :: fromParam.Out
        def apply(params: FieldType[Symbol @@ K, Param.Out[A]] :: Rest, params2: Map[String, Object]): Out =
          field[Symbol @@ K](params2(witness.value).asInstanceOf[A]) :: fromParam.apply(params.tail, params2)
      }
  }

  /** internal function works on homogeneous lists */
  private def callTx[R](conn: Connection, sql: String, params: Map[String, Param]): Map[String, Object] = {
    val cs = conn.prepareCall(sql)
    try {
      params.foreach {
        case (paramName, Param.In(p))                  => cs.setObject(paramName, p)
        case (paramName, Param.Out(sqlType, typeName)) => if (typeName.isEmpty) cs.registerOutParameter(paramName, sqlType)
                                                          else                  cs.registerOutParameter(paramName, sqlType, typeName)
        // case InOut => cs.registerOutParameter(paramName, sqlType, typeName); cs.setObject(paramName, p)
      }
      cs.execute()
      params.toList.flatMap {
        case (paramName, _: Param.In                 ) => List.empty
        case (paramName, Param.Out(sqlType, typeName)) => List(paramName -> cs.getObject(paramName))
      }.toMap
    } finally {
      cs.close()
    }
  }

  /** wrapper of `callTx` converting inputs and outputs to and from tagged HList and Map. */
  def call[L <: HList](conn: Connection, proc: String, l: L)(implicit fromParam: FromParam[L], toParam: ToParam[L]): fromParam.Out = {
    val params = toParam.apply(l)
    val procedure = params.map(_ => "?").mkString(s"{call $proc(", ", ", ")}")
    val res = callTx(conn, procedure, params)
    fromParam.apply(l, res)
  }

  /** intermediate trait to help infer types (currying) */
  trait CallAdt[R] {
    def apply[P, L <: HList, Out <: HList](conn: Connection, proc: String, p: P)(
      implicit
      inGeneric : LabelledGeneric.Aux[P, L]
    , fromParam : FromParam.Aux[L, Out]
    , toParam   : ToParam[L]
    , outGeneric: LabelledGeneric.Aux[R, Out]
    ): R
  }

  /** variant of call with inputs/outputs as Case classes, with properties matching procedure parameter names */
  def callAdt[R] =
    new CallAdt[R] {
      def apply[P, L <: HList, Out <: HList](conn: Connection, proc: String, p: P)(
        implicit
        inGeneric : LabelledGeneric.Aux[P, L]
      , fromParam : FromParam.Aux[L, Out]
      , toParam   : ToParam[L]
      , outGeneric: LabelledGeneric.Aux[R, Out]
        ): R = {
          val l = inGeneric.to(p)
          val res = call(conn, proc, l)
          outGeneric.from(res)
        }
    }
}