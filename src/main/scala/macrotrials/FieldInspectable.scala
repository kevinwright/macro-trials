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
    TheFlags <: InspectableFlags
  ]
  (using flags: TheFlags = InspectableFlags.PrimitivesOnly.given_InspectableFlags): FieldInspectable[P] =
    ${productMacro[P, TheFlags]}

  private def productMacro[
    P: Type,
    TheFlags <: InspectableFlags : Type,
  ](using Quotes): Expr[FieldInspectable[P]] =
    import quotes.reflect._
    val name = Type.show[P]
    val flagsType = TypeTree.of[TheFlags].tpe

    // RefinedType(
    //   parent = RefinedType(
    //     parent = TypeRef(
    //       repr = ThisType(TypeRef(NoPrefix,module class macrotrials)),
    //       name = trait InspectableFlags
    //     ),
    //     name = AnyValIsInspectable,
    //     info = TypeBounds(
    //       lo = TermRef(
    //         qual = ThisType(TypeRef(NoPrefix,module class macrotrials)),
    //         name = object EnabledFalse
    //       ),
    //       hi = TermRef(
    //         qual = ThisType(TypeRef(NoPrefix,module class macrotrials)),
    //         name = object EnabledFalse
    //       )
    //     )
    //   ),
    //   name = CaseClassIsInspectable,
    //   info = TypeBounds(
    //     lo = TermRef(
    //       qual = ThisType(TypeRef(NoPrefix,module class macrotrials)),
    //       name = object EnabledFalse
    //     ),
    //     hi = TermRef(
    //       qual = ThisType(TypeRef(NoPrefix,module class macrotrials)),
    //       name = object EnabledFalse
    //     )
    //   )
    // )

    // if true then println("true") else compiletime.error("false")

    def extractFlags: Map[String, Boolean] = 
      def loop(acc: Map[String, Boolean], tpe: TypeRepr): Map[String, Boolean] =
        tpe match {
          case Refinement(parent, name, TypeBounds(_,TermRef(_,enabledType))) =>
            val isEnabled = enabledType == "EnabledTrue"
            val nextAcc = acc + (name -> isEnabled)
            loop(nextAcc, parent)
          case _ => acc  
        }
      loop(Map.empty, TypeTree.of[TheFlags].tpe)

    val flags = extractFlags

    val anyValEnabled: Boolean = flags("AnyValIsInspectable")
    val caseClassEnabled: Boolean = flags("CaseClassIsInspectable")

    Type.of[P] match
      case '[AnyVal] =>
        if anyValEnabled then
          '{
            new FieldInspectable[P] {
              def inspect() = ${Expr(s"AnyVal: $name")}
            }
          }
        else '{compiletime.error("AnyVal inspection not enabled")}
      case _ =>
        if caseClassEnabled then
          '{
            new FieldInspectable[P] {
              def inspect() = ${Expr(s"Case Class:  $name")}
            }
          }
        else '{compiletime.error("case class inspection not enabled")}

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
