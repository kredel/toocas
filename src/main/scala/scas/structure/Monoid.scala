package scas.structure

trait Monoid[S <: Monoid[S]] extends SemiGroup[S] {
  type E <: Element
  def one = apply(1)
  def pow(x: E, exp: java.math.BigInteger) = {
    assert (exp.intValue() >= 0)
    (one /: (1 to exp.intValue())) { (l, r) => l * x }
  }
  trait Element extends super.Element { this: E =>
    def isUnit: Boolean
    def isOne = this >< one
  }
}
