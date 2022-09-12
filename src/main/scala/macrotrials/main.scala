package macrotrials

import ShowTree.showTree


case class InterestingClass(
  i: WrappedInt,
  i2: LegacyWrappedInt,
  s: String @IntAttr(42),
  d: Double @StrAttr("boo!")
)


@main def app() =
    val wi = WrappedInt(42)
    val wi2 = LegacyWrappedInt(42)

    summon[Ordering[Logarithms.Logarithm]]

    // println("The tree is: " + showTree(wi))
    // println("The tree is: " + showTree(WrappedInt(42)))
    // println("The tree is: " + showTree(wi2))
    // println("The tree is: " + showTree(LegacyWrappedInt(42)))

    println("FieldInspectables:")
    println(summon[FieldInspectable[Int]].inspect())
    println(summon[FieldInspectable[Float]].inspect())
    println(summon[FieldInspectable[Double]].inspect())
    println(summon[FieldInspectable[String]].inspect())
    println(summon[FieldInspectable[WrappedInt]].inspect())
    println(summon[FieldInspectable[LegacyWrappedInt]].inspect())
    // println(summon[FieldInspectable[InterestingClass]].inspect())
    println("==========")
    println()

    println(summon[Inspectable[InterestingClass]].inspect())
