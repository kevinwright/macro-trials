package macrotrials

opaque type WrappedInt <: Int = Int

object WrappedInt:
  def apply(i: Int): WrappedInt = i
  transparent inline given FieldInspectable[WrappedInt] = new FieldInspectable[WrappedInt] {
    type Underlying = Int
    override def summarise(): String = "WrappedInt"
    override def inspect(t: WrappedInt): String = s"WrappedInt(${t.toInt})"
    override def instance(u: Int): WrappedInt = WrappedInt(u)
  }

extension (x: WrappedInt)
  def toInt: Int = x
