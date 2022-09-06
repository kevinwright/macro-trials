
case class Attr(value: String) extends scala.annotation.StaticAnnotation

case class InterestingClass(i: Int, s: String, @Attr("boo!") d: Double)

@main def app() =
    println(Inspectable.inspect[InterestingClass])
    println("Hello, world")