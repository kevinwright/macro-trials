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

  inline def getElemLabels[A <: Tuple]: List[String] = inline erasedValue[A] match {
    case _: EmptyTuple => Nil // stop condition - the tuple is empty
    case _: (head *: tail) =>  // yes, in scala 3 we can match on tuples head and tail to deconstruct them step by step
        val headElementLabel = constValue[head].toString // bring the head label to value space
        val tailElementLabels = getElemLabels[tail] // recursive call to get the labels from the tail
        headElementLabel :: tailElementLabels // concat head + tail
    }


  inline def fieldTypeName[P](fieldName: String): String =
    ${fieldTypeNameImplMacro[P]('fieldName)}

  private def fieldTypeNameImplMacro[P](using Type[P])(using Quotes)(fieldNameExpr: Expr[String]): Expr[String] =
    import quotes.reflect._
    println(s"macrodebug: seeking ${fieldNameExpr.value}")
    fieldNameExpr.value match
      case Some(name) =>
        println(s"macrodebug: $name")
        val lookup = caseFieldTypes[P].toMap
        Expr(lookup(name))
      case _ => 
        // TODO - compile time exception
        report.error(s"Unable to find compile time fieldname from: ${fieldNameExpr.show}")
        '{???}

  private def caseFieldTypes[P](using Type[P])(using Quotes): Seq[(String, String)] =
    import quotes.reflect._
    val ts = TypeTree.of[P].tpe.typeSymbol
    for
      fieldSym <- ts.caseFields
      ValDef(name, tpe, optDefault) = fieldSym.tree
    yield
      name -> tpe.tpe.toString
        // //ts.caseFields.find(_.name == name).getOrElse(Symbol.noSymbol)
        // tpe.tpe match
        //   // TODO - Ensure this is a `@field(num)` annotation. 
        //   case AnnotatedType(tpe, Apply(term, List(Literal(IntConstant(num))))) =>
        //     (fieldSym.name, num.asInstanceOf[Int])
        //   case _ => (fieldSym.name, 0)

  inline def getElemTypeNames[P, M <: Mirror.ProductOf[P]](m: M, names: List[String]): List[String] = 
    names map {name =>
      // fieldTypeName[P]("i")
      fieldTypeName[P](name)
    }

  inline def derivedProduct[P, M <: Mirror.ProductOf[P]](m: M): Inspectable[P] =
    new Inspectable[P]:
      val label = constValue[m.MirroredLabel].toString
      val elemNames = getElemLabels[m.MirroredElemLabels]
      val elemTypeNames = getElemTypeNames[P, M](m, elemNames)
      override def inspect(): String =
        label + ": " + elemTypeNames.mkString("[", ", ", "] ") + elemNames.mkString("(", ", ", ")")


  inline def derivedSum[T, M <: Mirror.SumOf[T]](m: M): Inspectable[T] = 
    compiletime.error("Can't inspect sum types")

}
