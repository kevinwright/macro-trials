trait Inspectable[T] {
  def inspect(): String
}

object Inspectable {
  import deriving.*
  import scala.compiletime.*
  import scala.quoted.{Type, Expr, Quotes}

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

  // def tpeNmeMacro[A](using xt: Type[A])(using Quotes): Expr[String] = {
  //   val name = Type.show(xt)
  //   Expr(name)
  // }
  // inline def typeName[A]: String = ${tpeNmeMacro[A]}

  inline def getElemTypeNames[A <: Tuple]: List[String] = inline erasedValue[A] match {
    case _: EmptyTuple => Nil // stop condition - the tuple is empty
    case _: (head *: tail) =>  // yes, in scala 3 we can match on tuples head and tail to deconstruct them step by step
        val headName = Type.show[head] // bring the head label to value space
        val tailNames = getElemTypeNames[tail] // recursive call to get the labels from the tail
        headName :: tailNames // concat head + tail
    }

  inline def derivedProduct[T, M <: Mirror.ProductOf[T]](m: M): Inspectable[T] =
    new Inspectable[T]:
      val label = constValue[m.MirroredLabel].toString
      val elemNames = getElemLabels[m.MirroredElemLabels]
      val elemTypeNames = getElemTypeNames[m.MirroredElemTypes]
      override def inspect(): String =
        label + ": " + elemTypeNames.mkString(", ")


  inline def derivedSum[T, M <: Mirror.SumOf[T]](m: M): Inspectable[T] = 
    compiletime.error("Can't inspect sum types")

}
