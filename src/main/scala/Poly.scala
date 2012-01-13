class Poly[C <: Ring](val ring: C) extends Ring {
  type E = Element
  def zero = apply(ring.zero)
  def compare(x: E, y: E) = ring.compare(x.value, y.value)
  class Element(val value: ring.E) extends super.Element {
    def +(that: E) = apply(this.value+that.value)
    override def toString = value.toString
  }
  object Element {
    implicit def coef2poly[D <% ring.E](value: D) = apply(value)
  }
  def apply(value: ring.E): E = new Element(value)
}
