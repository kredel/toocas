package scas.structure

trait Ring extends AbelianGroup with Monoid {
  type E <: Element
  def characteristic: java.math.BigInteger
  trait Element extends super[AbelianGroup].Element with super[Monoid].Element { this: E => }
}
