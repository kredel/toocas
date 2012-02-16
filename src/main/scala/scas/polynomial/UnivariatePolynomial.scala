package scas.polynomial

import scas.structure.{Field, EuclidianDomain}

trait UnivariatePolynomial[C <: Field, @specialized(Int, Long) N] extends PolynomialOverUFD[C, N] with EuclidianDomain {
  import pp.variables
  assert (variables.length == 1)
  def norm(x: E) = degree(x)
  def derivative(w: E) = map(w, (a, b) => (a / pp.generator(0), b * ring.fromInt(n.toInt(pp.degree(a)))))
  def gcd1(x: E, y: E): E = if (y isZero) x else gcd1(y, monic(remainder(x, y)))
  override def content(x: E) = if (x isZero) ring.zero else headCoefficient(x)
  def monic(x: E) = primitivePart(x)
}
