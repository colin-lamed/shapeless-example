package api

import scala.language.dynamics
import scala.language.experimental.macros

import scala.reflect.macros.whitebox
import shapeless.SingletonTypeUtils

object ApiMacro extends Dynamic {
    def selectDynamic(tpeSelector: String): Any = macro ApiMacroBundle.theMacro
}

// need to be whitebox rather than blackbox, else leads to `stable identifier required, but ((): Any) found.`
class ApiMacroBundle(val c: whitebox.Context) extends SingletonTypeUtils {
  import c.universe.{ Symbol => _, _ }

  val regex = "\\{(.+):(.+)\\}".r

  def intersperse(sep: String, xs: Array[String]): Array[String] =
    xs.flatMap(x => Array(sep, x)).drop(1)

  def theMacro(tpeSelector: Tree): Tree = {
    println(s"macro: tpeSelector=$tpeSelector")
    val q"${tpeString: String}" = tpeSelector

    val elemTypes =
      intersperse("/", tpeString.split("/")).map { elemTypeStr =>
        elemTypeStr match {
          case regex(name, tpe) => val paramTpe = s"""Api.Param["$name", $tpe]"""
                                   parseStandardType(paramTpe)
                                     .getOrElse(c.abort(c.enclosingPosition, s"Malformed literal or standard type $paramTpe"))
          case name             => c.internal.constantType(Constant(name))
        }
      }

    val tpe =
      elemTypes.foldRight(hnilTpe) { case (elemTpe, acc) =>
        appliedType(hconsTpe, List(elemTpe, acc))
      }

    println(s"macro: tpe=$tpe")

    typeCarrier(tpe)
  }
}
