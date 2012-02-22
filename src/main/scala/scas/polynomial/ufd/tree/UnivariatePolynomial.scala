package scas.polynomial.ufd.tree

import scas.polynomial.ordering.Ordering
import scas.polynomial.{TreePolynomial, PowerProduct}
import scas.structure.Field
import TreePolynomial.Element

class UnivariatePolynomial[C, @specialized(Int, Long) N](val ring: Field[C], val pp: PowerProduct[N])(implicit val ordering: Ordering[N], val cm: ClassManifest[Element[C, N]]) extends scas.polynomial.ufd.UnivariatePolynomial[Element[C, N], C, N] with TreePolynomial[C, N]

object UnivariatePolynomial {
  def apply[C, @specialized(Int, Long) N](ring: Field[C], pp: PowerProduct[N])(implicit ordering: Ordering[N], cm: ClassManifest[Element[C, N]]) = new UnivariatePolynomial(ring, pp)
}
