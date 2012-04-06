package scas.polynomial.ufd.tree

import scala.collection.SortedMap
import scas.polynomial.{TreePolynomial, PowerProduct}
import scas.structure.UniqueFactorizationDomain
import scas.polynomial.ufd.PolynomialOverUFD
import PolynomialWithSubresGCD.Element

class PolynomialWithSubresGCD[C, @specialized(Int, Long) N](val ring: UniqueFactorizationDomain[C], val pp: PowerProduct[N])(implicit val cm: ClassManifest[Element[C, N]]) extends TreePolynomial[Element[C, N], C, N] with scas.polynomial.ufd.PolynomialWithSubresGCD[Element, C, N] {
  def split = PolynomialWithSubresGCD(PolynomialWithSubresGCD(ring, pp.take(location)), pp.drop(location))
  def apply(value: SortedMap[Array[N], C]) = new Element(value)(this)
}

object PolynomialWithSubresGCD {
  def apply[C, @specialized(Int, Long) N](ring: UniqueFactorizationDomain[C], pp: PowerProduct[N]) = new PolynomialWithSubresGCD(ring, pp)

  class Element[C, @specialized(Int, Long) N](val value: SortedMap[Array[N], C])(override val factory: PolynomialWithSubresGCD[C, N]) extends TreePolynomial.Element[Element[C, N], C, N] with PolynomialOverUFD.Element[Element[C, N], C, N]
  implicit def coef2polynomial[D, C, @specialized(Int, Long) N](value: D)(implicit f: D => C, factory: PolynomialWithSubresGCD[C, N]) = factory(value)
}