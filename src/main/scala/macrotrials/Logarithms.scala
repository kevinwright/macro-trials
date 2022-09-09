
package macrotrials

object Logarithms:
  opaque type Logarithm = Double

  object Logarithm:
    def apply(d: Double): Logarithm = math.log(d)
    given Ordering[Logarithm] with 
      def compare(a: Logarithm, b: Logarithm): Int = a.compareTo(b)


  extension (x: Logarithm)
    def toDouble: Double = math.exp(x)
    def compareTo(other: Logarithm) = x.toDouble.compareTo(other.toDouble)
    def + (y: Logarithm): Logarithm = Logarithm(math.exp(x) + math.exp(y))
    def * (y: Logarithm): Logarithm = x + y