package scas.polynomial.tree

import scala.collection.SortedMap
import scas.polynomial.ordering.Ordering
import scas.polynomial.{TreePolynomial, PowerProduct}
import scas.structure.Ring
import WeylAlgebra.Element

class WeylAlgebra[C, @specialized(Int, Long) N](val ring: Ring[C], val pp: PowerProduct[N])(implicit val ordering: Ordering[N], val cm: ClassManifest[Element[C, N]]) extends TreePolynomial[Element[C, N], C, N] with scas.polynomial.WeylAlgebra[Element[C, N], C, N] {
  def apply(value: SortedMap[Array[N], C]) = new Element(value)(this)
}

object WeylAlgebra {
  def apply[C, @specialized(Int, Long) N](ring: Ring[C], pp: PowerProduct[N])(implicit ordering: Ordering[N]) = new WeylAlgebra(ring, pp)

  class Element[C, @specialized(Int, Long) N](val value: SortedMap[Array[N], C])(override val factory: WeylAlgebra[C, N]) extends TreePolynomial.Element[Element[C, N], C, N]
  implicit def coef2polynomial[D, C, @specialized(Int, Long) N](value: D)(implicit f: D => C, factory: WeylAlgebra[C, N]) = factory(value)
}
