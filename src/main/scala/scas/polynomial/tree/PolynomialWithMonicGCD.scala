package scas.polynomial.tree

import scas.polynomial.PowerProduct
import scas.structure.Field
import MultivariatePolynomial.Element

class PolynomialWithMonicGCD[C, @specialized(Int, Long) N](override val ring: Field[C], override val pp: PowerProduct[N])(implicit cm: ClassManifest[Element[C, N]]) extends MultivariatePolynomial(ring, pp) with scas.polynomial.PolynomialWithMonicGCD[Element, C, N] {
  override def split = MultivariatePolynomial(MultivariatePolynomial.withMonicGCD(ring, pp.take(location)), pp.drop(location))
}
