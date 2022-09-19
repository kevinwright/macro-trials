package macrotrials

import scala.annotation.{implicitNotFound, targetName}
import scala.deriving.Mirror
import MacroUtils.*

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

  transparent inline given [
    P <: Product,
    AnyValInspectable <: InspectableFlag[AnyVal],
    ProductInspectable <: InspectableFlag[Product],
  ]
  (using x: AnyValInspectable = InspectableFlag.default[AnyVal])
  (using y: ProductInspectable = InspectableFlag.default[Product]): FieldInspectable[P] =
    ${productMacro[P, AnyValInspectable, ProductInspectable]}

  private def productMacro[
    P: Type,
    AnyValInspectable <: InspectableFlag[AnyVal] : Type,
    ProductInspectable <: InspectableFlag[Product] : Type,
  ](using Quotes): Expr[FieldInspectable[P]] =
    import quotes.reflect._
    val name = Type.show[P]
    
    // println(Type.show[AnyValEnabled])
    // println(TypeTree.of[ProductInspectable].tpe)

    val anyValEnabled: Boolean = TypeTree.of[AnyValInspectable].tpe match {
      case Refinement(parent, name, TypeBounds(_,TermRef(_,"EnabledTrue"))) => true
      case _ => false
    }

    val productEnabled: Boolean = TypeTree.of[ProductInspectable].tpe match {
      case Refinement(parent, name, TypeBounds(_,TermRef(_,"EnabledTrue"))) => true
      case _ => false
    }

    println(s"AnyVal enabled = $anyValEnabled, Product enabled = $productEnabled")

    Type.of[P] match
      case '[AnyVal] =>
        '{
          new FieldInspectable[P] {
            def inspect() = ${Expr(s"AnyVal: $name - $anyValEnabled")}
          }
        }
      case _ =>
        '{
          new FieldInspectable[P] {
            def inspect() = ${Expr(s"Case Class:  $name - $productEnabled")}
          }
        }

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


/*


  object AnyValInstance:
    import scala.util.NotGiven


    // inline given [P <: Product with AnyVal]: FieldInspectable[P] =
    //   ${anyValMacro[P]}
    transparent inline given [P <: Product](using NotGiven[Mirror.ProductOf[P]]): FieldInspectable[P] =
      ${anyValMacro[P]}

    private def anyValMacro[P](using Type[P])(using Quotes): Expr[FieldInspectable[P]] =
      import quotes.reflect._
      val name = Type.show[P]
      '{
        new FieldInspectable[P] {
          def inspect() = ${Expr("AnyVal: " + name)}
        }
      }

    // inline given [
    //   T <: Product with AnyVal,
    //   M <: Mirror.ProductOf[T]
    // ]: FieldInspectable[T] = () => "AnyVal: " + typeName[T]

    // inline given [
    //   T <: Product with AnyVal,
    //   M <: Mirror.ProductOf[T]
    // ]: FieldInspectable[T] =
    //   val m = compiletime.summonInline[M]
    //   () => "AnyVal: " + typeName[T]

    // inline given [
    //   T <: Product with AnyVal
    // ](using m: Mirror.ProductOf[T]): FieldInspectable[T] = () => "AnyVal: " + typeName[T]

    // inline given [
    //   T <: Product with AnyVal,
    //   M <: Mirror.ProductOf[T]
    // ]: FieldInspectable[T] = 
    //   inline compiletime.erasedValue[M.MirroredElemTypes] match
    //     case _: (fieldType *: fieldTypes) => 
    //       () => "AnyVal: " + typeName[T] + " wrapping " + typeName[fieldType]
    //     case _: EmptyTuple => () => compiletime.error("???")  

    // inline given [T <: Product]
    //   (using T <:< AnyVal)
    //   (using m: Mirror.Of[T]): FieldInspectable[T] =
    //     inline compiletime.erasedValue[m.MirroredElemTypes] match
    //       case _: (fieldType *: fieldTypes) => 
    //         () => "AnyVal: " + typeName[T] + " wrapping " + typeName[fieldType]
    //       case _: EmptyTuple => compiletime.error("???")  

*/
