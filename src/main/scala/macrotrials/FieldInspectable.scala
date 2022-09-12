package macrotrials

import scala.annotation.implicitNotFound

@implicitNotFound("Could not find an implicit FieldInspectable[${T}]")
trait FieldInspectable[T]:
  def inspect(): String

object FieldInspectable:

  inline given FieldInspectable[Int] = () => "Int"
  inline given FieldInspectable[Long] = () => "Long"
  inline given FieldInspectable[Float] = () => "Float"
  inline given FieldInspectable[Double] = () => "Double"
  inline given FieldInspectable[String] = () => "String"

  inline given [T <: Product]: FieldInspectable[Product] = () => "Product"
  inline given FieldInspectable[Any] = () => "Any"
