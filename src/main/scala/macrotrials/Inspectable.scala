package macrotrials

trait Inspectable[T] {
  def inspect(): String
}

object Inspectable {
  import deriving.*
  import scala.compiletime.*
  import scala.quoted.*

  def inspect[CC <: Product](using ins: Inspectable[CC]): String = ins.inspect()
  
  inline given derived[T](using m: Mirror.Of[T]): Inspectable[T] =
    inline m match
      case m: Mirror.ProductOf[T] =>
        derivedProduct[T, m.type](m)
      case m: Mirror.SumOf[T] =>
        derivedSum[T, m.type](m)
      case _ => compiletime.error("Cannot derive Marshallable for non-struct classes")

  inline def summonFor[T]: Inspectable[T] =
    summonFrom {
      case b: Inspectable[T] => b
      // TODO - this is terrible, figure out a way not to
      // waste so much compute for this.
      case m: Mirror.Of[T] => derived[T]
      case _ => compiletime.error("Unable to construct Inspectable")
    }
  
  inline def mkFieldInfo[ProdType, MemberLbl, MemberType]: FieldInfo =
    ${mkFieldInfoImplMacro[ProdType, MemberLbl, MemberType]}

  inline def enquote(s: String) = s"\"s\""
  inline def dequote(s: String) = s.stripPrefix("\"").stripSuffix("\"")

  private def mkFieldInfoImplMacro[
    ProdType: Type,
    MemberLbl: Type,
    MemberType: Type
  ](using Quotes): Expr[FieldInfo] =
    import quotes.reflect._

    val caseFieldTypes = determineCaseFieldTypes[ProdType]
    
    val fieldName = dequote(TypeTree.of[MemberLbl].show)
    val fieldType = TypeTree.of[MemberType].show
    println(s"macrodebug: seeking $fieldName: $fieldType")

    // println("typerepr: " + TypeRepr.of[MemberType].typeSymbol.annotations.map(_.tpe.show))

    val annotationInSymbol = caseFieldTypes collectFirst {
      case (name, AnnotatedType(_, Apply(Select(fnType, _), params))) if name == fieldName =>
        s"${fnType.tpe.show}" -> params.map(_.show)
    }
    // annotationInSymbol foreach {
    //   case (name, params) => println(s"annotationInSymbol: $name(${params.mkString(", ")})")
    // }
    // println("======")

    // println(TypeTree.of[T])
    // println(TypeTree.of[T].tpe)

    val fieldNameExpr = Expr(fieldName)
    val fieldTypeExpr = Expr(fieldType)
    annotationInSymbol match {
      case None =>
        '{
          FieldInfo(
            name = $fieldNameExpr,
            typeName = $fieldTypeExpr,
            annotation = None
          )
        }
      case Some(name, params) =>
        '{
          FieldInfo(
            name = $fieldNameExpr,
            typeName = $fieldTypeExpr,
            annotation = Some(
              FieldAnnotation(
                className = ${Expr(name)},
                params = ${Expr(params)}
              )
            )
          )
        }
    }
    


  private def determineCaseFieldTypes[P](using Type[P])(using q: Quotes): Seq[(String, q.reflect.TypeRepr)] =
    import quotes.reflect._
    val ts = TypeTree.of[P].tpe.typeSymbol
    for
      fieldSym <- ts.caseFields
      ValDef(name, tpe, _) = fieldSym.tree
    yield
      name -> tpe.tpe

  inline private def inspectFields[ProductType, FieldTypes <: Tuple, Labels <: Tuple]: List[FieldInfo] =
    inline erasedValue[FieldTypes] match
      case _: (fieldType *: fieldTypes) =>
        inline erasedValue[Labels] match
          case _: (fieldLabel *: fieldLabels) =>
            val fieldInfo = mkFieldInfo[ProductType, fieldLabel, fieldType]
            fieldInfo :: inspectFields[ProductType, fieldTypes, fieldLabels]
          case _: EmptyTuple => compiletime.error("LOGIC ERROR: Ran out of field labels for field types")
      case _: EmptyTuple => Nil  

  inline def derivedProduct[P, M <: Mirror.ProductOf[P]](m: M): Inspectable[P] =
    new Inspectable[P]:
      val className = constValue[m.MirroredLabel].toString
      val fieldInfos = inspectFields[P ,m.MirroredElemTypes, m.MirroredElemLabels ]
      override def inspect(): String =
        className + ": " + fieldInfos.mkString("[", ", ", "] ")


  inline def derivedSum[T, M <: Mirror.SumOf[T]](m: M): Inspectable[T] = 
    compiletime.error("Can't inspect sum types")

}
