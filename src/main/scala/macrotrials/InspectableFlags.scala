package macrotrials

sealed trait EnabledFlag
object EnabledTrue extends EnabledFlag
object EnabledFalse extends EnabledFlag

trait InspectableFlag[T] {
  type Enabled <: EnabledFlag
}

object InspectableFlag  {
  transparent inline def default[T]: InspectableFlag[T] =
    new InspectableFlag[T] { type Enabled = EnabledFalse.type }
}

object InspectableFlags {

  object AnyValFieldIsInspectable:
    transparent inline given InspectableFlag[AnyVal] =
      new InspectableFlag[AnyVal] { type Enabled = EnabledTrue.type }

  object CaseClassFieldIsInspectable:
    transparent inline given InspectableFlag[Product] =
      new InspectableFlag[Product] { type Enabled = EnabledTrue.type }

}
