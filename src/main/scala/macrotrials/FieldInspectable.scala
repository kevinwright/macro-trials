package macrotrials

import scala.annotation.{implicitNotFound, targetName}
import scala.deriving.Mirror
import MacroUtils.*

@implicitNotFound(
  "Could not find an implicit FieldInspectable[${T}]\n" +
  "You may be missing an `InspectableFlags` instance\n" +
  "Try one of the following:\n" +
  " - import macrotrials.InspectableFlags.PrimitivesOnly.given\n" +
  " - import macrotrials.InspectableFlags.Mixed.given\n" +
  " - import macrotrials.InspectableFlags.FullMonty.given"
)
trait FieldInspectable[T]:
  def inspect(): String

object FieldInspectable:

  import scala.quoted.*

  inline given FieldInspectable[Int] = () => "Int"
  inline given FieldInspectable[Long] = () => "Long"
  inline given FieldInspectable[Float] = () => "Float"
  inline given FieldInspectable[Double] = () => "Double"
  inline given FieldInspectable[String] = () => "String"

  transparent inline given [P <: Product, TheFlags <: InspectableFlags]
  (using flags: TheFlags): FieldInspectable[P] =
    ${productMacro[P, TheFlags]}

  private def productMacro[
    P: Type,
    TheFlags <: InspectableFlags : Type,
  ](using Quotes): Expr[FieldInspectable[P]] =
    import quotes.reflect._
    val className = Type.show[P]
    val flagsType = TypeTree.of[TheFlags].tpe

    // RefinedType(
    //   parent = RefinedType(
    //     parent = TypeRef(
    //       repr = ThisType(TypeRef(NoPrefix,module class macrotrials)),
    //       name = trait InspectableFlags
    //     ),
    //     name = AnyValIsInspectable,
    //     info = TypeBounds(
    //       lo = TypeRef(
    //         repr = ThisType(TypeRef(NoPrefix,module class macrotrials)),
    //         name = class EnabledTrue
    //       ),
    //       hi = TypeRef(
    //         repr = ThisType(TypeRef(NoPrefix,module class macrotrials)),
    //         name = class EnabledTrue
    //       )
    //     )
    //   ),
    //   name = CaseClassIsInspectable,
    //   info = TypeBounds(
    //     lo = TypeRef(
    //       repr = ThisType(TypeRef(NoPrefix,module class macrotrials)),
    //       name = class EnabledFalse
    //     ),
    //     hi = TypeRef(
    //       repr = ThisType(TypeRef(NoPrefix,module class macrotrials)),
    //       name = class EnabledFalse
    //     )
    //   )
    // )
    

    // Every type member in `TheFlags` adds another level of nesting to the resulting
    // `RefinedType` tree, so we drag 'em all out using recursion

    val flags =
      def loop(acc: Map[String, Boolean], tpe: TypeRepr): Map[String, Boolean] =
        tpe match {
          case Refinement(parent, flagName, TypeBounds(_,TypeRef(_,enabledType))) =>
            val isEnabled = enabledType == "EnabledTrue"
            val nextAcc = acc + (flagName -> isEnabled)
            loop(nextAcc, parent)
          case _ => acc  
        }
      loop(Map.empty, TypeTree.of[TheFlags].tpe)

    val anyValEnabled: Boolean = flags("AnyValIsInspectable")
    val caseClassEnabled: Boolean = flags("CaseClassIsInspectable")

    Type.of[P] match
      case '[AnyVal] =>
        if anyValEnabled then
          TypeTree.of[P].tpe.typeSymbol.caseFields.head.tree match {
            case ValDef(valueName, tpt, rhs) =>
              val AppliedType(reference, args) = TypeRepr.of[FieldInspectable[Int]]: @unchecked
              val wanted = AppliedType(reference, List(tpt.tpe))

              Implicits.search(wanted) match {
                case iss: ImplicitSearchSuccess =>
                  // println(s"P tree success = ${iss.tree}}")
                  val nestedExpr = iss.tree.asExpr.asInstanceOf[Expr[FieldInspectable[_]]]
                  val classNameExpr = Expr(className)
                  val valueNameExpr = Expr(valueName)
                  '{
                    new FieldInspectable[P] {
                      val nestedInspect: String = $nestedExpr.inspect()
                      val className: String = $classNameExpr
                      val valueName: String = $valueNameExpr
                      def inspect() = s"AnyVal: $className($valueName: $nestedInspect)"
                    }
                  }
                case isf: ImplicitSearchFailure =>
                  '{compiletime.error(${Expr(isf.explanation)})}
              }
            case _ => '{compiletime.error("This is uncomfortable")}
          }
        else '{compiletime.error("AnyVal inspection not enabled")}
      case _ =>
        if caseClassEnabled then
          '{
            new FieldInspectable[P] {
              def inspect() = ${Expr(s"Case Class: $className")}
            }
          }
        else '{compiletime.error("case class inspection not enabled")}

  object UnknownInstance:
    inline given [T]: FieldInspectable[T] = ${unknownMacro[T]}

    private def unknownMacro[T](using Type[T])(using Quotes): Expr[FieldInspectable[T]] =
      import quotes.reflect._
      val name = TypeTree.of[T].show
      '{
        new FieldInspectable[T] {
          def inspect() = ${Expr(name)}
        }
      }


