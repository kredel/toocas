package scas.polynomial.tree

import scas.polynomial.{PowerProduct, PolynomialOverField}
import scas.structure.Field
import MultivariatePolynomial.Element

class MultivariatePolynomialOverField[C, @specialized(Int, Long) N](override val ring: Field[C], override val pp: PowerProduct[N])(implicit cm: ClassManifest[Element[C, N]]) extends PolynomialWithPrimitiveGCD(ring, pp) with PolynomialOverField[Element[C, N], C, N] {
  override def split = MultivariatePolynomial.withPrimitiveGCD(MultivariatePolynomial.overField(ring, pp.take(location)), pp.drop(location))
}
