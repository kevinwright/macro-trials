package macrotrials

opaque type WrappedInt = Int

object WrappedInt:
  def apply(i: Int): WrappedInt = i
  inline given FieldInspectable[WrappedInt] = () => "WrappedInt"

extension (x: WrappedInt)
  def toInt: Int = x
