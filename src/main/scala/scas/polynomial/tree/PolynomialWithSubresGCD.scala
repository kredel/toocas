package scas.polynomial.tree

import scas.polynomial.PowerProduct
import scas.structure.UniqueFactorizationDomain
import MultivariatePolynomial.Element

class PolynomialWithSubresGCD[C, @specialized(Int, Long) N](ring: UniqueFactorizationDomain[C], pp: PowerProduct[N])(implicit cm: ClassManifest[Element[C, N]]) extends MultivariatePolynomial(ring, pp) with scas.polynomial.PolynomialWithSubresGCD[Element, C, N] {
  def split = MultivariatePolynomial.withSubresGCD(MultivariatePolynomial.withSubresGCD(ring, pp.take(location)), pp.drop(location))
}
