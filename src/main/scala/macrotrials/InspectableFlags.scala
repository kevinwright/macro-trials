package macrotrials

import scala.annotation.implicitNotFound

sealed trait EnabledFlag
final class EnabledTrue extends EnabledFlag
final class EnabledFalse extends EnabledFlag

@implicitNotFound("No given instance of InspectableFlags has been imported, try `import macrotrials.PrimitivesOnly.given` or `import macrotrials.FullMonty.given`")
trait InspectableFlags {
  type AnyValIsInspectable <: EnabledFlag
  type CaseClassIsInspectable <: EnabledFlag
}

object InspectableFlags {
  object PrimitivesOnly:
    transparent inline given InspectableFlags =
      new InspectableFlags {
        type AnyValIsInspectable = EnabledFalse
        type CaseClassIsInspectable = EnabledFalse
      }

  object FullMonty:
    transparent inline given InspectableFlags =
      new InspectableFlags {
        type AnyValIsInspectable = EnabledTrue
        type CaseClassIsInspectable = EnabledTrue
      }

  object Mixed:
    transparent inline given InspectableFlags =
      new InspectableFlags {
        type AnyValIsInspectable = EnabledTrue
        type CaseClassIsInspectable = EnabledFalse
      }

}
