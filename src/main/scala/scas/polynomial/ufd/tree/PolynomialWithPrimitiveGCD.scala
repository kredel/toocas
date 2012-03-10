package scas.polynomial.ufd.tree

import scala.collection.SortedMap
import scas.polynomial.ordering.Ordering
import scas.polynomial.{TreePolynomial, PowerProduct}
import scas.structure.UniqueFactorizationDomain
import PolynomialWithPrimitiveGCD.Element

class PolynomialWithPrimitiveGCD[C, @specialized(Int, Long) N](val ring: UniqueFactorizationDomain[C], val pp: PowerProduct[N])(implicit val ordering: Ordering[N], val cm: ClassManifest[Element[C, N]]) extends scas.polynomial.ufd.PolynomialWithPrimitiveGCD[Element, C, N] with TreePolynomial[Element[C, N], C, N] {
  def split = PolynomialWithPrimitiveGCD(PolynomialWithPrimitiveGCD(ring, pp.take(location)), pp.drop(location))
  def apply(value: SortedMap[Array[N], C]) = new Element(value)(this)
}

object PolynomialWithPrimitiveGCD {
  def apply[C, @specialized(Int, Long) N](ring: UniqueFactorizationDomain[C], pp: PowerProduct[N])(implicit ordering: Ordering[N], cm: ClassManifest[Element[C, N]]) = new PolynomialWithPrimitiveGCD(ring, pp)

  class Element[C, @specialized(Int, Long) N](val value: SortedMap[Array[N], C])(override val factory: PolynomialWithPrimitiveGCD[C, N]) extends scas.polynomial.ufd.PolynomialOverUFD.Element[Element[C, N], C, N] with TreePolynomial.Element[Element[C, N], C, N]
  implicit def coef2polynomial[D, C, @specialized(Int, Long) N](value: D)(implicit f: D => C, factory: PolynomialWithPrimitiveGCD[C, N]) = factory(value)
}
