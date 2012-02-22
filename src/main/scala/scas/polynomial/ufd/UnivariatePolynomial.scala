package scas.polynomial.ufd

import scas.polynomial.Polynomial
import scas.structure.{Field, EuclidianDomain, UniqueFactorizationDomain}
import UniqueFactorizationDomain.Implicits.infixUFDOps
import Polynomial.Element

trait UnivariatePolynomial[T <: Element[T, C, N], C, N] extends PolynomialOverUFD[T, C, N] with EuclidianDomain[T] {
  override implicit val ring: Field[C]
  assert (pp.length == 1)
  def norm(x: T) = java.math.BigInteger.valueOf(degree(x))
  def derivative(w: T) = map(w, (a, b) => (a / pp.generator(0), b * ring(pp.degree(a))))
  def gcd1(x: T, y: T): T = if (y isZero) x else gcd1(y, monic(remainder1(x, y)))
  override def content(x: T) = if (x isZero) ring.zero else headCoefficient(x)
  def monic(x: T) = primitivePart(x)
}
