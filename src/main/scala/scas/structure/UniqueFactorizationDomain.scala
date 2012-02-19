package scas.structure

trait UniqueFactorizationDomain[S <: UniqueFactorizationDomain[S]] extends Ring[S] {
  type E <: Element
  def gcd(x: E, y: E): E
  def lcm(x: E, y: E) = (x * y) / gcd(x, y)
  trait Element extends super.Element { this: E =>
    def /  (that: E): E
    def %  (that: E): E
    def /% (that: E): (E, E)
    def |  (that: E) = (that % this) isZero
  }
}
