package persistence

import java.sql.Connection
import shapeless.{HNil, HList, ::}


/** Invoke db procedure with HList inputs/outputs.
  * The position in the HList corresponds to the position of the parameter in the procedure
  */
object PersistenceByIndex {

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
  // ToParam converts HList to a List of Param.
  // First look for Param.In or Param.Out adding into List.
  // Then assume any other values should be wrapped in Param.In before adding into List.
  //

  trait ToParam[L <: HList] {
    def apply(params : L): List[Param]
  }

  trait LowerPrecedenceToParam {
    implicit def assumeInToParam[A, Rest <: HList](implicit toParam: ToParam[Rest]) =
      new ToParam[A :: Rest] {
        def apply(params: A :: Rest): List[Param] =
          Param.In(params.head) :: toParam.apply(params.tail)
      }
  }

  object ToParam extends LowerPrecedenceToParam {
    implicit val nilToParam = new ToParam[HNil] {
      def apply(params: HNil): List[Param] = Nil
    }

    implicit def inToParam[A, Rest <: HList](implicit toParam: ToParam[Rest]) =
      new ToParam[Param.In :: Rest] {
        def apply(params: Param.In :: Rest): List[Param] =
          params.head :: toParam.apply(params.tail)
      }

    implicit def outToParam[A, Rest <: HList](implicit toParam: ToParam[Rest]) =
      new ToParam[Param.Out[A] :: Rest] {
        def apply(params: Param.Out[A] :: Rest): List[Param] =
          params.head :: toParam.apply(params.tail)
      }
  }

  //
  // FromParam converts List of results into HList, getting the type from the initial HList.
  // First try Param.Out[A] first, adding the output value to HList.
  // Then assume any other values are inputs, and skip output.
  //

  trait FromParam[L <: HList] {
    type Out <: HList
    def apply(params : L, params2: List[Either[Unit, Object]]): Out
  }

  trait LowerPrecedenceFromParam {
    implicit def inFromParam[A, Rest <: HList](implicit fromParam: FromParam[Rest]) =
      new FromParam[A :: Rest] {
        type Out = fromParam.Out
        def apply(params: A :: Rest, params2: List[Either[Unit, Object]]): Out =
          fromParam.apply(params.tail, params2.tail)
      }
 }

  object FromParam extends LowerPrecedenceFromParam {
    implicit val nilFromParam = new FromParam[HNil] {
      type Out = HNil
      def apply(params: HNil, params2: List[Either[Unit, Object]]): Out = HNil
    }

    implicit def outFromParam[A, Rest <: HList](implicit fromParam: FromParam[Rest]) =
      new FromParam[Param.Out[A] :: Rest] {
        type Out = A :: fromParam.Out
        def apply(params: Param.Out[A] :: Rest, params2: List[Either[Unit, Object]]): Out =
          params2.head.right.get.asInstanceOf[A] :: fromParam.apply(params.tail, params2.tail)
      }
  }

  /** internal function works on homogeneous lists */
  private def callTx[R](conn: Connection, sql: String, params: List[Param]): List[Either[Unit,Object]] = {
    val cs = conn.prepareCall(sql)
    try {
      params.zipWithIndex.foreach {
        case (Param.In(p), i)                  => cs.setObject(i + 1, p)
        case (Param.Out(sqlType, typeName), i) => if (typeName.isEmpty) cs.registerOutParameter(i + 1, sqlType)
                                                  else                  cs.registerOutParameter(i + 1, sqlType, typeName)
        // case InOut => cs.registerOutParameter(i + 1, sqlType, typeName); cs.setObject(i + 1, p)
      }
      cs.execute()
      params.zipWithIndex.map {
        case (_: Param.In, _)                  => Left(())
        case (Param.Out(sqlType, typeName), i) => Right(cs.getObject(i + 1))
      }
    } finally {
      cs.close()
    }
  }

  /** wrapper of `callTx` converting inputs and outputs to and from HList and List. */
  def call[L <: HList](conn: Connection, proc: String, l: L)(implicit fromParam: FromParam[L], toParam: ToParam[L]): fromParam.Out = {
    val params = toParam.apply(l)
    val procedure = params.map(_ => "?").mkString(s"{call $proc(", ", ", ")}")
    val res = callTx(conn, procedure, params)
    fromParam.apply(l, res)
  }
}