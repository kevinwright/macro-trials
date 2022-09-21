package macrotrials

import scala.annotation.{implicitNotFound, targetName}
import scala.deriving.Mirror
import MacroUtils.*

@implicitNotFound(
  "Could not find an implicit FieldInspectable[${T}]\n" +
  "You may be missing an `InspectableFlags` instance\n" +
  "Try one of the following:\n" +
  " - import macrotrials.InspectableFlags.PrimitivesOnly.given\n" +
  " - import macrotrials.InspectableFlags.Neptune.given\n" +
  " - import macrotrials.InspectableFlags.FullMonty.given"
)
trait FieldInspectable[T]:
  def summarise(): String
  def inspect(t: T): String

object FieldInspectable:

  def summarise[T](using ins: FieldInspectable[T]): String = ins.summarise()
  def inspect[T](value: T)(using ins: FieldInspectable[T]): String = ins.inspect(value)

  import scala.quoted.*
  import scala.compiletime.erasedValue

  inline given FieldInspectable[Int] = new FieldInspectable[Int] {
    override def summarise(): String = "Int"
    override def inspect(t: Int): String = s"$t: Int"
  }
  inline given FieldInspectable[Long] = new FieldInspectable[Long] {
    override def summarise(): String = "Long"
    override def inspect(t: Long): String = s"$t: Long"
  }
  inline given FieldInspectable[Float] = new FieldInspectable[Float] {
    override def summarise(): String = "Float"
    override def inspect(t: Float): String = s"$t: Float"
  }
  inline given FieldInspectable[Double] = new FieldInspectable[Double] {
    override def summarise(): String = "Double"
    override def inspect(t: Double): String = s"$t: Double"
  }
  inline given FieldInspectable[String] = new FieldInspectable[String] {
    override def summarise(): String = "String"
    override def inspect(t: String): String = s"$t: String"
  }
      
  transparent inline given [T, Coll[_] <: Iterable[_], TheFlags <: InspectableFlags]
    (using flags: TheFlags)
    (using nested: FieldInspectable[T]): FieldInspectable[Coll[T]] =
      inline erasedValue[Coll[T]] match
        case _: Seq[T] =>
          inline erasedValue[flags.SeqIsInspectable] match
            case _: EnabledTrue => 
              new FieldInspectable[Seq[T]] {
                override def summarise(): String =
                  s"Seq[${nested.summarise()}]"
                override def inspect(t: Seq[T]): String =
                  t.map(nested.inspect).mkString("Seq(", ",", ")")
              }.asInstanceOf[FieldInspectable[Coll[T]]]
            case _ => compiletime.error("Seq inspection not enabled") 
        case _: Set[T] =>
          inline erasedValue[flags.SetIsInspectable] match
            case _: EnabledTrue => 
              new FieldInspectable[Set[T]] {
                override def summarise(): String =
                  s"Set[${nested.summarise()}]"
                override def inspect(t: Set[T]): String =
                  t.map(nested.inspect).mkString("Set(", ",", ")")
              }.asInstanceOf[FieldInspectable[Coll[T]]]
            case _ => compiletime.error("Set inspection not enabled")     
        case _ => compiletime.error("Unknown collection type")       

  inline given [K, V, TheFlags <: InspectableFlags]
    (using flags: TheFlags)
    (using keyIns: FieldInspectable[K], valIns: FieldInspectable[V]): FieldInspectable[Map[K,V]] =
      inline erasedValue[flags.MapIsInspectable] match
        case _: EnabledTrue => 
          new FieldInspectable[Map[K, V]] {
            override def summarise(): String =
              s"Map[${keyIns.summarise()}, ${valIns.summarise()}]"
            override def inspect(t: Map[K, V]): String =
              t.map((k,v) => keyIns.inspect(k) + "=" + valIns.inspect(v)).mkString("Map(", ",", ")")
          }
        case _ => compiletime.error("Seq inspection not enabled") 

  inline given [P <: Product, TheFlags <: InspectableFlags]
  (using flags: TheFlags): FieldInspectable[P] =
    ${productMacro[P, TheFlags]}

  private def productMacro[
    P: Type,
    TheFlags <: InspectableFlags : Type,
  ](using Quotes): Expr[FieldInspectable[P]] =
    import quotes.reflect.*
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

    def nestedInstanceFor(tt: TypeRepr): AppliedType =
      val AppliedType(reference, _) = TypeRepr.of[FieldInspectable[_]]: @unchecked
      AppliedType(reference, List(tt))     

    def withNestedInstance[R](tt: TypeRepr)(fn: Expr[FieldInspectable[_]] => Expr[R]): Expr[R] =
      Implicits.search(nestedInstanceFor(tt)) match {
        case iss: ImplicitSearchSuccess =>
          val instance = iss.tree.asExpr.asInstanceOf[Expr[FieldInspectable[_]]]
          fn(instance)
        case isf: ImplicitSearchFailure =>
          report.errorAndAbort(isf.explanation)
      }

    Type.of[P] match
      case '[AnyVal] =>
        if anyValEnabled then
          TypeTree.of[P].tpe.typeSymbol.caseFields.head.tree match {
            case ValDef(valueName, tpt, rhs) =>
              withNestedInstance(tpt.tpe) { nestedExpr =>
                val classNameExpr = Expr(className)
                val valueNameExpr = Expr(valueName)
                '{
                  new FieldInspectable[P] {
                    val nestedSummary: String = $nestedExpr.summarise()
                    val className: String = $classNameExpr
                    val valueName: String = $valueNameExpr
                    override def summarise(): String = s"AnyVal: $className($valueName: $nestedSummary)"
                    override def inspect(t: P): String = ???
                  }
                }
              }
            case _ => report.errorAndAbort("AnyVal doesn't appear to have a constructor param")
          }
        else report.errorAndAbort("AnyVal inspection not enabled")
      case _ =>
        if caseClassEnabled then
          '{
            new FieldInspectable[P] {
              override def summarise(): String = ${Expr(s"Case Class: $className")}
              override def inspect(t: P): String = ???
            }
          }
        else report.errorAndAbort("case class inspection not enabled")

  object UnknownInstance:
    inline given [T]: FieldInspectable[T] = ${unknownMacro[T]}

    private def unknownMacro[T](using Type[T])(using Quotes): Expr[FieldInspectable[T]] =
      import quotes.reflect.*
      val name = TypeTree.of[T].show
      '{
        new FieldInspectable[T] {
          override def summarise(): String = ${Expr(name)}
          override def inspect(t: T): String = ???
        }
      }


