package scas.structure

trait Ring[S <: Ring[S]] extends AbelianGroup[S] with Monoid[S] {
  type E <: Element
  def characteristic: java.math.BigInteger
  trait Element extends super[AbelianGroup].Element with super[Monoid].Element { this: E => }
}
