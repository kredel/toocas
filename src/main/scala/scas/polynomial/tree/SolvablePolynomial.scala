package scas.polynomial.tree

import scala.collection.SortedMap
import scas.polynomial.ordering.Ordering
import scas.polynomial.{TreePolynomial, PowerProduct}
import scas.structure.Ring
import SolvablePolynomial.Element

class SolvablePolynomial[C, @specialized(Int, Long) N](val ring: Ring[C], val pp: PowerProduct[N])(implicit val ordering: Ordering[N], val cm: ClassManifest[Element[C, N]]) extends TreePolynomial[Element[C, N], C, N] with scas.polynomial.SolvablePolynomial[Element[C, N], C, N] {
  def apply(value: SortedMap[Array[N], C]) = new Element(value)(this)
}

object SolvablePolynomial {
  def apply[C, @specialized(Int, Long) N](ring: Ring[C], pp: PowerProduct[N])(implicit ordering: Ordering[N], cm: ClassManifest[Element[C, N]]) = new SolvablePolynomial(ring, pp)

  class Element[C, @specialized(Int, Long) N](val value: SortedMap[Array[N], C])(override val factory: SolvablePolynomial[C, N]) extends TreePolynomial.Element[Element[C, N], C, N]
  implicit def coef2polynomial[D, C, @specialized(Int, Long) N](value: D)(implicit f: D => C, factory: SolvablePolynomial[C, N]) = factory(value)
}
