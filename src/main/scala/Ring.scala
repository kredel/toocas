trait Ring {
  type E <: Element
  def zero: E
  def compare(x: E, y: E): Int
  trait Element extends Ordered[E] { this: E =>
    def +(that: E): E
    def compare(that: E) = Ring.this.compare(this, that)
  }
}

object Ring {
  implicit def ring2ordering(ring: Ring) = new Ordering[ring.E] {
    def compare(x: ring.E, y: ring.E) = ring.compare(x, y)
  }
}
