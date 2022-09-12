package macrotrials

import scala.annotation.{implicitNotFound, targetName}
import scala.deriving.Mirror


@implicitNotFound("Could not find an implicit FieldInspectable[${T}]")
trait FieldInspectable[T]:
  def inspect(): String

object FieldInspectable:

  import scala.quoted.*

  inline given FieldInspectable[Int] = () => "Int"
  inline given FieldInspectable[Long] = () => "Long"
  inline given FieldInspectable[Float] = () => "Float"
  inline given FieldInspectable[Double] = () => "Double"
  inline given FieldInspectable[String] = () => "String"

  object UnknownInstance:
    // @targetName("given_FieldInspectable_Product")
    // inline given [T <: Product]: FieldInspectable[T] = () => "Product"
    inline given [T]: FieldInspectable[T] = ${unknownMacro[T]}


    private def unknownMacro[T](using Type[T])(using Quotes): Expr[FieldInspectable[T]] =
      import quotes.reflect._
      val name = TypeTree.of[T].show
      '{
        new FieldInspectable[T] {
          def inspect() = ${Expr(name)}
        }
      }

  object ProductInstance:
    import scala.util.NotGiven

    inline given [
      P <: Product,
      M <: Mirror.ProductOf[P]
    ](using NotGiven[P <:< AnyVal]): FieldInspectable[P] = ${productMacro[P, M]}


    private def productMacro[
      P,
      M <: Mirror.ProductOf[P]
    ](using Type[P])(using Quotes): Expr[FieldInspectable[P]] =
      import quotes.reflect._
      val name = TypeTree.of[P].show
      '{
        new FieldInspectable[P] {
          def inspect() = ${Expr("Product: " + name)}
        }
      }