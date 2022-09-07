final case class FieldInfo(
  name: String,
  typeName: String,
) {
  override def toString(): String = s"$name: $typeName"
}
