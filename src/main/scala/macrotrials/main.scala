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

    //import InspectableFlags.Neptune.given
    import InspectableFlags.FullMonty.given
    // import InspectableFlags.Testing.given

    // println(summon[InspectableFlags])
    println("Summaries:")
    println(FieldInspectable.summarise[Int])
    println(FieldInspectable.summarise[Float])
    println(FieldInspectable.summarise[Double])
    println(FieldInspectable.summarise[String])
    println(FieldInspectable.summarise[WrappedInt])
    println(FieldInspectable.summarise[LegacyWrappedInt])
    println(FieldInspectable.summarise[List[Int]])
    println(FieldInspectable.summarise[Set[Int]])
    println(FieldInspectable.summarise[Map[String, LegacyWrappedInt]])
    println(FieldInspectable.summarise[InterestingClass])
    println("==========")
    println()

    println("Inspections:")
    println(FieldInspectable.inspect(42))
    println(FieldInspectable.inspect(42.0f))
    println(FieldInspectable.inspect(42.0d))
    println(FieldInspectable.inspect("foo"))
    println(FieldInspectable.inspect(WrappedInt(42)))
    // println(FieldInspectable.inspect(LegacyWrappedInt(42)))
    println(FieldInspectable.inspect(List(1,2,3)))
    println(FieldInspectable.inspect(Set(1,2,3)))
    println(FieldInspectable.inspect(Map("a" -> 1, "b" -> 2)))
    // println(summon[FieldInspectable[InterestingClass]].summarise())
    println("==========")
    println()
