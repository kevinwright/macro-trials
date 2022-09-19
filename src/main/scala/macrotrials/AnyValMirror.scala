package macrotrials

import scala.deriving.Mirror
import MacroUtils.*
import scala.compiletime
import compiletime.erasedValue
import scala.quoted.*

case class AnyValMirror[T](
  className: String,
  valueName: String,
  valueTypeName: String,
)

object AnyValMirror:
  inline given [T <: Product with AnyVal]: AnyValMirror[T] =
    ${ImplMacro[T]}

  private def ImplMacro[T](using Type[T])(using Quotes): Expr[AnyValMirror[T]] =
    import quotes.reflect._
    val className = Type.show[T]
    val ts = TypeTree.of[T].tpe.typeSymbol

    val (valueName: String, valueTypeName: String) = ts.caseFields.head.tree match {
      case ValDef(name, tpe, _) =>  name -> tpe.show
      case _ => "unknown" -> "unknown"
    }

    '{
      AnyValMirror[T](
        className = ${Expr(className)},
        valueName = ${Expr(valueName)},
        valueTypeName = ${Expr(valueTypeName)},
      )
    }    


   

