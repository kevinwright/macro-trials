package macrotrials

final case class LegacyWrappedInt(val value: Int) extends AnyVal

object LegacyWrappedInt:
  inline given FieldInspectable[LegacyWrappedInt] = () => "LegacyWrappedInt"
