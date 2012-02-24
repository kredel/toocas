package scas.polynomial.ufd.tree

import scala.collection.SortedMap
import scas.polynomial.ordering.Ordering
import scas.polynomial.{TreePolynomial, PowerProduct}
import scas.polynomial.ufd.PolynomialOverUFD
import scas.structure.Field
import UnivariatePolynomial.Element

class UnivariatePolynomial[C, @specialized(Int, Long) N](val ring: Field[C], val pp: PowerProduct[N])(implicit val ordering: Ordering[N], val cm: ClassManifest[Element[C, N]]) extends scas.polynomial.ufd.UnivariatePolynomial[Element[C, N], C, N] with TreePolynomial[Element[C, N], C, N] {
  def apply(value: SortedMap[Array[N], C]) = new Element(value)(this)
}

object UnivariatePolynomial {
  def apply[C, @specialized(Int, Long) N](ring: Field[C], pp: PowerProduct[N])(implicit ordering: Ordering[N], cm: ClassManifest[Element[C, N]]) = new UnivariatePolynomial(ring, pp)

  class Element[C, @specialized(Int, Long) N](val value: SortedMap[Array[N], C])(override val factory: UnivariatePolynomial[C, N]) extends PolynomialOverUFD.Element[Element[C, N], C, N] with TreePolynomial.Element[Element[C, N], C, N]
  implicit def coef2polynomial[D, C, @specialized(Int, Long) N](value: D)(implicit f: D => C, factory: UnivariatePolynomial[C, N]) = factory(value)
}
