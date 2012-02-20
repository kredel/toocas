package scas.polynomial.ufd

import scas.structure.{Field, EuclidianDomain}
import scas.ZZ

trait UnivariatePolynomial[S <: UnivariatePolynomial[S, C, N], C <: Field[C], @specialized(Int, Long) N] extends PolynomialOverUFD[S, C, N] with EuclidianDomain[S] {
  assert (pp.length == 1)
  def norm(x: E) = ZZ(degree(x))
  def derivative(w: E) = map(w, (a, b) => (a / pp.generator(0), b * ring(pp.degree(a))))
  def gcd1(x: E, y: E): E = if (y isZero) x else gcd1(y, monic(remainder(x, y)))
  override def content(x: E) = if (x isZero) ring.zero else headCoefficient(x)
  def monic(x: E) = primitivePart(x)
}
