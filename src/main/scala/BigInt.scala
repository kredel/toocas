object BigInt extends Ring {
  type E = Element
  def zero = apply(0)
  def compare(x: E, y: E) = x.value compare y.value
  class Element(val value: Int) extends super.Element {
    def +(that: E) = apply(this.value+that.value)
    override def toString = value.toString
  }
  object Element {
    implicit def int2bigInt(value: Int) = apply(value)
  }
  def apply(value: Int): E = new Element(value)
}
