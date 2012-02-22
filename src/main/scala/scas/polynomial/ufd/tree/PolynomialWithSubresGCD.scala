package scas.polynomial.ufd.tree

import scas.polynomial.ordering.Ordering
import scas.polynomial.{TreePolynomial, PowerProduct}
import scas.structure.UniqueFactorizationDomain
import TreePolynomial.Element

class PolynomialWithSubresGCD[C, @specialized(Int, Long) N](val ring: UniqueFactorizationDomain[C], val pp: PowerProduct[N])(implicit val ordering: Ordering[N], val cm: ClassManifest[Element[C, N]]) extends scas.polynomial.ufd.PolynomialWithSubresGCD[Element, C, N] with TreePolynomial[C, N] {
  def split = new PolynomialWithSubresGCD(new PolynomialWithSubresGCD(ring, pp.take(location)), pp.drop(location))
}

object PolynomialWithSubresGCD {
  def apply[C, @specialized(Int, Long) N](ring: UniqueFactorizationDomain[C], pp: PowerProduct[N])(implicit ordering: Ordering[N], cm: ClassManifest[Element[C, N]]) = new PolynomialWithSubresGCD(ring, pp)
}
