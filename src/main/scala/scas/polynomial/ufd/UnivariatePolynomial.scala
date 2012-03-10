package scas.polynomial.ufd

import scas.polynomial.PowerProduct
import scas.structure.{EuclidianDomain, UniqueFactorizationDomain}
import UniqueFactorizationDomain.Implicits.infixUFDOps
import PowerProduct.Implicits.infixPowerProductOps
import PolynomialOverUFD.Element

trait UnivariatePolynomial[T <: Element[T, C, N], C, N] extends PolynomialOverField[T, C, N] with EuclidianDomain[T] {
  assert (length == 1)
  def norm(x: T) = java.math.BigInteger.valueOf(degree(x))
  def derivative(w: T) = map(w, (a, b) => (a / pp.generator(0), b * ring(pp.degree(a))))
}
