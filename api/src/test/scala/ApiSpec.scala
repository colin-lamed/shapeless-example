package api

import org.scalatest.FlatSpec
import shapeless.{CNil, HNil, Inl, Inr, ::, :+:}
import shapeless.labelled.FieldType
import shapeless.tag.@@

class ApiSpec extends FlatSpec {
  import Api._

  case class Output(
    name: String
  , age : Int
  )

  "Api" should "match empty" in {
    def parse = parseUrl[HNil, HNil]
    assert(parse("") === Right(HNil))
    assert(parse("asd") === Left("'asd' was not empty"))
  }

  it should "match string" in {
    def parse = parseUrl["hello" :: HNil, HNil]
    assert(parse("hello") === Right(HNil))
    assert(parse("asd") === Left("'asd' does not start with 'hello'"))
  }

  it should "extract param" in {
    def parse = parseUrl[ Param["name", String] :: HNil
                        , FieldType[Symbol @@ "name", String] :: HNil
                        ]
    assert(parse("Bill") === Right("Bill" :: HNil))
  }

  it should "extract param2" in {
    def parse = parseUrl[ Param["name", String] :: "/" :: HNil
                        , FieldType[Symbol @@ "name", String] :: HNil
                        ]
    assert(parse("Bill/") === Right("Bill" :: HNil))
  }

  it should "extract param3" in {
    def parse = parseUrl[ "name" :: "/" :: Param["name", String] :: "/" :: "age" :: "/" :: Param["age", Int] :: HNil
                        , FieldType[Symbol @@ "name", String] :: FieldType[Symbol @@ "age", Int] :: HNil
                        ]
    assert(parse("name/Bill/age/12") === Right("Bill" :: 12 :: HNil))
  }

  it should "extract hlist input directly to ADT" in {

    type Route = "name" :: "/" :: Param["name", String] :: "/" :: "age" :: "/" :: Param["age", Int] :: HNil

    def parse = parseUrlToRecord[Route, Output]

    val e = parse("name/Bill/age/12")
    assert(e.isRight)
    val Right(i) = e
    assert(i.name === "Bill")
    assert(i.age === 12)
  }

  it should "extract hlist input with macro type description" in {

    type Route = ApiMacro.`name/{name:String}/age/{age:Int}`.T

    def parse = parseUrlToRecord[Route, Output]

    val e = parse("name/Bill/age/12")
    assert(e.isRight)
    val Right(i) = e
    assert(i.name === "Bill")
    assert(i.age === 12)
  }

  it should "extract coproduct input" in {

    sealed trait Output
    final case class Person(
      name: String
    , age : Int
    ) extends Output
    final case class Address(
      number: Int
    , street: String
    ) extends Output

    type AddressRoute = "number" :: "/" :: Param["number", Int] :: "/" :: "street" :: "/" :: Param["street", String] :: HNil
    type PersonRoute  = "name" :: "/" :: Param["name", String] :: "/" :: "age" :: "/" :: Param["age", Int] :: HNil
    type Route = AddressRoute :+: PersonRoute :+: CNil

    // model type which can be converted to Output with LabelledGenerics
    type Record = Address :+: Person :+: CNil

    import LabelledGenericInstances._

    // parseUrlToRecord returns Coproduct, which can then be converted to ADT
    def parse(s: String): Either[String, Output] =
      parseUrlToRecord[Route, Record](s).map {
        case Inl(a)      => a
        case Inr(Inl(p)) => p
        case Inr(Inr(_)) => ???
      }


    {
      val e = parse("name/Bill/age/12")
      assert(e.isRight)
      val Right(i) = e
      i match {
        case p: Person => assert(p.name === "Bill")
                          assert(p.age === 12)
        case other     => fail(s"was $other")
      }
    }

    {
      val e = parse("number/3/street/Lavender Close")
      assert(e.isRight)
      val Right(i) = e
      i match {
        case a: Address => assert(a.number === 3)
                           assert(a.street === "Lavender Close")
        case other      => fail(s"was $other")
      }
    }
  }

  it should "extract coproduct input directly to ADT" in {

    sealed trait Output
    final case class Person(
      name: String
    , age : Int
    ) extends Output
    final case class Address(
      number: Int
    , street: String
    ) extends Output

    type AddressRoute = "number" :: "/" :: Param["number", Int] :: "/" :: "street" :: "/" :: Param["street", String] :: HNil
    type PersonRoute  = "name" :: "/" :: Param["name", String] :: "/" :: "age" :: "/" :: Param["age", Int] :: HNil
    type Route = AddressRoute :+: PersonRoute :+: CNil

    import LabelledGenericInstances._


    def parse = parseUrlToAdt[Route, Output]

    {
      val e = parse("name/Bill/age/12")
      assert(e.isRight)
      val Right(i) = e
      i match {
        case p: Person => assert(p.name === "Bill")
                          assert(p.age === 12)
        case other     => fail(s"was $other")
      }
    }

    {
      val e = parse("number/3/street/Lavender Close")
      assert(e.isRight)
      val Right(i) = e
      i match {
        case a: Address => assert(a.number === 3)
                           assert(a.street === "Lavender Close")
        case other      => fail(s"was $other")
      }
    }
  }
}