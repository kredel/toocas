package scas.polynomial.ufd.tree

import scas.polynomial.ordering.Ordering
import scas.polynomial.PowerProduct
import scas.structure.UniqueFactorizationDomain
import PolynomialWithSimpleGCD.Element

object MultivariatePolynomial {
  def apply[C, @specialized(Int, Long) N](ring: UniqueFactorizationDomain[C], pp: PowerProduct[N])(implicit ordering: Ordering[N], cm: ClassManifest[Element[C, N]]) = new PolynomialWithSimpleGCD(ring, pp)
}
