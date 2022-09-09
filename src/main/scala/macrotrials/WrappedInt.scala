package macrotrials

opaque type WrappedInt = Int

object WrappedInt:
  def apply(i: Int): WrappedInt = i

extension (x: WrappedInt)
  def toInt: Int = x
