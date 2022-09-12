package macrotrials

import scala.annotation.implicitNotFound

@implicitNotFound("Could not find an implicit FieldInspectable[${T}]")
trait FieldInspectable[T]:
  def inspect(): String

object FieldInspectable:

  // inline given [Int]: FieldInspectable[Int] = () => "Int"
  // inline given [Long]: FieldInspectable[Long] = () => "Long"
  // inline given [Float]: FieldInspectable[Float] = () => "Float"
  inline given [Double]: FieldInspectable[Double] = () => "Double"
  // inline given [String]: FieldInspectable[String] = () => "String"

  inline given [AnyRef <: Product ]: FieldInspectable[AnyRef] = () => "T"
