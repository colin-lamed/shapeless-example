package api

import scala.util.Try
import shapeless.{CNil, Coproduct, HNil, HList, Inl, Inr, LabelledGeneric, Lazy, Witness, ::, :+:}
import shapeless.labelled.{field, FieldType}
import shapeless.ops.{coproduct, union}
import shapeless.tag.@@


/** Given a Route defined as HList of String and Param, a url can be parsed and validated against this.
  * Supports HLists and Coproduct, and also converting inputs/outputs to case classes.
  */
object Api {

  case class Param[I <: String, O]() // only exists for the type (TODO best way? Proxy?)


  trait ParseUrl[In] {
    type Out
    def apply(s: String): Either[String, Out]
  }

  object ParseUrl {
    type Aux[I, O] = ParseUrl[I] { type Out = O }

    def apply[I, O](implicit ev: ParseUrl.Aux[I, O]): ParseUrl.Aux[I, O] =
      ev

    implicit val nilParseUrl =
      new ParseUrl[HNil] {
        type Out = HNil
        def apply(s: String): Either[String, Out] =
          if (s.isEmpty) Right(HNil)
          else Left (s"'$s' was not empty")
      }

    implicit def strParseUrl[S <: String, Rest <: HList](implicit witness: Witness.Aux[S], ev: ParseUrl[Rest]) =
      new ParseUrl[S :: Rest] {
        type Out = ev.Out
        def apply(s: String): Either[String, Out] =
          if (s.startsWith(witness.value)) ev.apply(s.stripPrefix(witness.value))
          else Left(s"'$s' does not start with '${witness.value}'")
      }

    def paramParseUrl[I <: String, Rest <: HList, O, RestOut <: HList](convert: String => Either[String, O])(implicit ev: ParseUrl.Aux[Rest, RestOut]) =
      new ParseUrl[Param[I, O] :: Rest] {
        type Out = FieldType[Symbol @@ I, O] :: RestOut
        def apply(s: String): Either[String, Out] = {
          val end = if (s.indexOf("/") != -1) s.indexOf("/") else s.length
          val res = s.substring(0, end)
          for {
            i    <- convert(res)
            rest <- ev.apply(s.substring(end))
          } yield field[Symbol @@ I](i) :: rest
        }
      }

    implicit def strParamParseUrl[I <: String, Rest <: HList, RestOut <: HList](implicit ev: ParseUrl.Aux[Rest, RestOut]) =
      paramParseUrl[I, Rest, String, RestOut](res => Right(res))

    implicit def intParamParseUrl[I <: String, Rest <: HList, RestOut <: HList](implicit ev: ParseUrl.Aux[Rest, RestOut]) =
      paramParseUrl[I, Rest, Int, RestOut](res =>
        Try(res.toInt).fold[Either[String, Int]](_ => Left(s"Invalid Int '$res'"), Right.apply)
      )

    implicit val cnilParseUrl =
      new ParseUrl[CNil] {
        type Out = CNil
        def apply(s: String): Either[String, Out] =
          throw new Exception("Inconceivable!")
      }


    implicit def coproductParseUrl[H, T <: Coproduct, HO, TO <: Coproduct](
      implicit
        hInstance: Lazy[ParseUrl.Aux[H, HO]] // wrap in Lazy
      , tInstance: ParseUrl.Aux[T, TO]
      ): ParseUrl.Aux[H :+: T, HO :+: TO] =
        new ParseUrl[H :+: T] {
          type Out = HO :+: TO
          def apply(s: String): Either[String, Out] = {
            hInstance.value.apply(s) match {
              case Left(err1) => tInstance.apply(s) match {
                case Left(err2) => Left("Matched neither: " + err1 + ", " + err2) // TODO use Validated for errors (this will accumulate with other Coproducts)
                case Right(to)  => Right(Inr(to))
              }
              case Right(ho)  => Right(Inl(ho))
            }
          }
        }
  }

  // shapeless is missing some LabelledGeneric definitions? (coproductLG, cnilLG)
  // or using them wrongly?
  object LabelledGenericInstances {
    implicit def coproductLG[O, ORest <: shapeless.Coproduct, OL, OLRest <: shapeless.Coproduct](
      implicit
          oLG: Lazy[LabelledGeneric.Aux[O, OL]]
        , rG : LabelledGeneric.Aux[ORest, OLRest]
        ): LabelledGeneric.Aux[O :+: ORest, OL :+: OLRest] =
      new LabelledGeneric[O :+: ORest] {
        type Repr = OL :+: OLRest

        def to(t: O :+: ORest): Repr =
            t.eliminate(
              l => Inl(oLG.value.to(l))
            , r => Inr(rG.to(r))
            )

        def from(repr: Repr): O :+: ORest =
          repr.eliminate(
              l => Inl(oLG.value.from(l))
            , r => Inr(rG.from(r))
            )
      }

    implicit val cnilLG: LabelledGeneric.Aux[CNil, CNil] =
      new LabelledGeneric[CNil] {
        type Repr = CNil

        def to(t: CNil): Repr = sys.error("Shouldn't happen")

        def from(r: Repr): CNil = sys.error("Shouldn't happen")
      }
  }


  // The Api

  def parseUrl[Route, Generic](implicit ev: ParseUrl.Aux[Route, Generic]): String => Either[String, Generic] =
    (s: String) => ev.apply(s)

  trait ParseUrlToRecord[Route, Record] {
    def apply[Generic](s: String)(
      implicit
        ev     : ParseUrl.Aux[Route, Generic]
      , generic: LabelledGeneric.Aux[Record, Generic]
      ): Either[String, Record]
  }

  def parseUrlToRecord[Route, Record]: ParseUrlToRecord[Route, Record] =
    new ParseUrlToRecord[Route, Record] {
      def apply[Generic](s: String)(
        implicit
          ev     : ParseUrl.Aux[Route, Generic]
        , generic: LabelledGeneric.Aux[Record, Generic]
        ) =
      parseUrl(ev)(s).map(generic.from)
    }

  trait ParseUrlToAdt[Route, ADT] {
    def apply[Record, RecordCoproduct <: Coproduct, K <: HList, V <: Coproduct](s: String)(
      implicit
        ev      : ParseUrl.Aux[Route, Record]
      , generic : LabelledGeneric.Aux[ADT, RecordCoproduct]
      , keys    : union.Keys.Aux[RecordCoproduct, K]
      , values  : union.Values.Aux[RecordCoproduct, V]
      , generic1: LabelledGeneric.Aux[V, Record]
      , zip     : coproduct.ZipWithKeys.Aux[K, V, RecordCoproduct]
      ): Either[String, ADT]
  }

  def parseUrlToAdt[Route, ADT]: ParseUrlToAdt[Route, ADT] =
    new ParseUrlToAdt[Route, ADT] {
      def apply[Record, RecordCoproduct <: Coproduct, K <: HList, V <: Coproduct](s: String)(
        implicit
          ev      : ParseUrl.Aux[Route, Record]
        , generic : LabelledGeneric.Aux[ADT, RecordCoproduct]
        , keys    : union.Keys.Aux[RecordCoproduct, K]
        , values  : union.Values.Aux[RecordCoproduct, V]
        , generic1: LabelledGeneric.Aux[V, Record]
        , zip     : coproduct.ZipWithKeys.Aux[K, V, RecordCoproduct]
        ): Either[String, ADT] =
      parseUrl(ev)(s).map(generic1.from).map(zip.apply).map(generic.from)
    }
}