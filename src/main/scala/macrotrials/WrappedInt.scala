package macrotrials

opaque type WrappedInt <: Int = Int

object WrappedInt:
  def apply(i: Int): WrappedInt = i
  inline given FieldInspectable[WrappedInt] = new FieldInspectable[WrappedInt] {
    override def summarise(): String = "WrappedInt"
    override def inspect(t: WrappedInt): String = s"WrappedInt(${t.toInt})"
  }

extension (x: WrappedInt)
  def toInt: Int = x
