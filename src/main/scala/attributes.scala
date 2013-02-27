package adept

trait Attribute
trait Attributed extends Attribute {
  def attr(key: String): Option[Attribute]
}
case class StringAttribute(value: String) extends Attribute
case class BoolAttribute(value: Boolean) extends Attribute
case class IntAttribute(value: Int) extends Attribute
case class DoubleAttribute(value: Double) extends Attribute
case class DecimalAttribute(value: BigDecimal) extends Attribute
case class Attributes(value: List[Attribute]) extends Attribute
