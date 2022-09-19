package macrotrials

sealed trait EnabledFlag
object EnabledTrue extends EnabledFlag
object EnabledFalse extends EnabledFlag

trait InspectableFlags {
  type AnyValIsInspectable <: EnabledFlag
  type CaseClassIsInspectable <: EnabledFlag
}

object InspectableFlags {
  object PrimitivesOnly:
    transparent inline given InspectableFlags =
      new InspectableFlags {
        type AnyValIsInspectable = EnabledFalse.type
        type CaseClassIsInspectable = EnabledFalse.type
      }

  object FullMonty:
    transparent inline given InspectableFlags =
      new InspectableFlags {
        type AnyValIsInspectable = EnabledTrue.type
        type CaseClassIsInspectable = EnabledTrue.type
      }

  object Mixed:
    transparent inline given InspectableFlags =
      new InspectableFlags {
        type AnyValIsInspectable = EnabledTrue.type
        type CaseClassIsInspectable = EnabledFalse.type
      }

}
