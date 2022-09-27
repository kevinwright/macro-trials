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
  type InspectedT = T
  type Underlying
  def summarise(): String
  def inspect(t: InspectedT): String
  def instance(u: Underlying): T

object FieldInspectable:

  def summarise[T](using ins: FieldInspectable[T]): String = ins.summarise()
  def inspect[T](value: T)(using ins: FieldInspectable[T]): String = ins.inspect(value)

  import scala.quoted.*
  import scala.compiletime.erasedValue

  transparent inline given FieldInspectable[Int] = new FieldInspectable[Int] {
    type Underlying = Int
    override def summarise(): String = "Int"
    override def inspect(t: Int): String = s"$t: Int"
    override def instance(x: Underlying): Underlying = x
  }
  transparent inline given FieldInspectable[Long] = new FieldInspectable[Long] {
    type Underlying = Long
    override def summarise(): String = "Long"
    override def inspect(t: Long): String = s"$t: Long"
    override def instance(x: Underlying): Underlying = x
  }
  transparent inline given FieldInspectable[Float] = new FieldInspectable[Float] {
    type Underlying = Float
    override def summarise(): String = "Float"
    override def inspect(t: Float): String = s"$t: Float"
    override def instance(x: Underlying): Underlying = x
  }
  transparent inline given FieldInspectable[Double] = new FieldInspectable[Double] {
    type Underlying = Double
    override def summarise(): String = "Double"
    override def inspect(t: Double): String = s"$t: Double"
    override def instance(x: Underlying): Underlying = x
  }
  transparent inline given FieldInspectable[String] = new FieldInspectable[String] {
    type Underlying = String
    override def summarise(): String = "String"
    override def inspect(t: String): String = s"$t: String"
    override def instance(x: Underlying): Underlying = x
  }
      
  transparent inline given [T, Coll[_] <: Iterable[_], TheFlags <: InspectableFlags]
    (using flags: TheFlags)
    (using nested: FieldInspectable[T]): FieldInspectable[Coll[T]] =
      inline erasedValue[Coll[T]] match
        case _: Seq[T] =>
          inline erasedValue[flags.SeqIsInspectable] match
            case _: EnabledTrue => 
              new FieldInspectable[Seq[T]] {
                type Underlying = Seq[T]
                override def summarise(): String =
                  s"Seq[${nested.summarise()}]"
                override def inspect(t: Seq[T]): String =
                  t.map(nested.inspect).mkString("Seq(", ",", ")")
                override def instance(x: Underlying): Underlying = x
              }.asInstanceOf[FieldInspectable[Coll[T]] {type Underlying = Seq[T]}]
            case _ => compiletime.error("Seq inspection not enabled") 
        case _: Set[T] =>
          inline erasedValue[flags.SetIsInspectable] match
            case _: EnabledTrue => 
              new FieldInspectable[Set[T]] {
                type Underlying = Set[T]

                override def summarise(): String =
                  s"Set[${nested.summarise()}]"
                override def inspect(t: Set[T]): String =
                  t.map(nested.inspect).mkString("Set(", ",", ")")
                override def instance(x: Underlying): Underlying = x
              }.asInstanceOf[FieldInspectable[Coll[T]] {type Underlying = Set[T]}]
            case _ => compiletime.error("Set inspection not enabled")     
        case _ => compiletime.error("Unknown collection type")       

  transparent inline given [K, V, TheFlags <: InspectableFlags]
    (using flags: TheFlags)
    (using keyIns: FieldInspectable[K], valIns: FieldInspectable[V]): FieldInspectable[Map[K,V]] =
      inline erasedValue[flags.MapIsInspectable] match
        case _: EnabledTrue => 
          new FieldInspectable[Map[K, V]] {
            type Underlying = Map[K, V]

            override def summarise(): String =
              s"Map[${keyIns.summarise()}, ${valIns.summarise()}]"
            override def inspect(t: Map[K, V]): String =
              t.map((k,v) => keyIns.inspect(k) + "=" + valIns.inspect(v)).mkString("Map(", ",", ")")
            override def instance(x: Underlying): Underlying = x
          }
        case _ => compiletime.error("Seq inspection not enabled") 

  transparent inline given [P <: Product, TheFlags <: InspectableFlags]
  (using flags: TheFlags): FieldInspectable[P] =
    ${productMacro[P, TheFlags]}

  private def productMacro[
    P: Type,
    TheFlags <: InspectableFlags : Type
  ](using Quotes): Expr[FieldInspectable[P]] =
    new FieldInspectableMacros[P, TheFlags].handleProduct

  class FieldInspectableMacros[
    P: Type,
    TheFlags <: InspectableFlags : Type,
  ](using Quotes):
    import quotes.reflect.*

    private val className = Type.show[P]
    private val flagsType = TypeTree.of[TheFlags].tpe

    // Every type member in `TheFlags` adds another level of nesting to the resulting
    // `RefinedType` tree, so we drag 'em all out using recursion

    private val flags =
      def loop(acc: Map[String, Boolean], tpe: TypeRepr): Map[String, Boolean] =
        tpe match {
          case Refinement(parent, flagName, TypeBounds(_,TypeRef(_,enabledType))) =>
            val isEnabled = enabledType == "EnabledTrue"
            val nextAcc = acc + (flagName -> isEnabled)
            loop(nextAcc, parent)
          case _ => acc  
        }
      loop(Map.empty, TypeTree.of[TheFlags].tpe)

    private val anyValEnabled: Boolean = flags("AnyValIsInspectable")
    private val caseClassEnabled: Boolean = flags("CaseClassIsInspectable")

    def handleProduct: Expr[FieldInspectable[P]] =
      Type.of[P] match
        case '[AnyVal] =>
          if anyValEnabled then handleAnyVal
          else report.errorAndAbort("AnyVal inspection not enabled")
        case _ =>
          if caseClassEnabled then
            '{
              new FieldInspectable[P] {
                type Underlying = P
                override def summarise(): String = ${Expr(s"Case Class: $className")}
                override def inspect(t: P): String = ???
                def instance(u: Underlying): P = ???
              }
            }
          else report.errorAndAbort("case class inspection not enabled")

    private def nestedInstanceFor[R](tt: TypeRepr): Expr[FieldInspectable[_]] =
      TypeRepr.of[FieldInspectable[_]] match
        case AppliedType(reference, _) =>
          val sought = AppliedType(reference, List(tt))
          Implicits.search(sought) match
            case iss: ImplicitSearchSuccess =>
              iss.tree.asExprOf[FieldInspectable[_]]
            case isf: ImplicitSearchFailure =>
              report.errorAndAbort(isf.explanation)
        case other => report.errorAndAbort(s"Unexpected: ${other.show}")

    private def handleAnyVal: Expr[FieldInspectable[P]] =
      val classSym: Symbol = TypeTree.of[P].tpe.typeSymbol

      val getterSym: Symbol = classSym.caseFields.head
      val companionSym = classSym.companionModule

      getterSym.tree match {
        case ValDef(valueName, tpt, _) =>
          val nestedTypeRepr = tpt.tpe
          val nestedType = nestedTypeRepr.asType
          val nestedInspectableExpr = nestedInstanceFor(nestedTypeRepr)
          nestedType match {
            case '[underlying] =>
              val companionIdent = Ident(companionSym.termRef)
              val companionApply = Select.unique(companionIdent, "apply")
              val classNameExpr = Expr(className)
              val valueNameExpr = Expr(valueName)
              '{
                new FieldInspectable[P] {
                  val nestedInspectable = $nestedInspectableExpr
                  type Underlying = underlying
                  val nestedSummary: String = nestedInspectable.summarise()
                  def nestedInspection(x: P): String =
                    nestedInspectable.inspect(
                      ${
                        '{x}.asTerm.select(getterSym).asExpr
                      }.asInstanceOf[nestedInspectable.InspectedT]
                    )
                  val className: String = $classNameExpr
                  val valueName: String = $valueNameExpr
                  override def summarise(): String =
                    s"AnyVal: $className($valueName: $nestedSummary)"
                  override def inspect(t: P): String =
                    s"AnyVal: $className($valueName: ${nestedInspection(t)})"
                  def instance(x: underlying): P =
                    ${
                      val arg = '{x}.asTerm
                      Apply(companionApply, arg :: Nil).asExprOf[P]
                    }
                }
              }
          }
        case _ => report.errorAndAbort("AnyVal doesn't appear to have a constructor param")
      }

  object UnknownInstance:
    transparent inline given [T]: FieldInspectable[T] = ${unknownMacro[T]}

    private def unknownMacro[T](using Type[T])(using Quotes): Expr[FieldInspectable[T]] =
      import quotes.reflect.*
      val name = TypeTree.of[T].show
      '{
        new FieldInspectable[T] {
          type Underlying = T
          override def summarise(): String = ${Expr(name)}
          override def inspect(t: T): String = ???
          def instance(u: Underlying): T = ???
        }
      }


