package scas.polynomial.tree

import scas.polynomial.PowerProduct
import scas.structure.UniqueFactorizationDomain
import MultivariatePolynomial.Element

class PolynomialWithPrimitiveGCD[C, @specialized(Int, Long) N](ring: UniqueFactorizationDomain[C], pp: PowerProduct[N])(implicit cm: ClassManifest[Element[C, N]]) extends MultivariatePolynomial(ring, pp) with scas.polynomial.PolynomialWithPrimitiveGCD[Element, C, N] {
  def split = MultivariatePolynomial.withPrimitiveGCD(MultivariatePolynomial.withPrimitiveGCD(ring, pp.take(location)), pp.drop(location))
}
