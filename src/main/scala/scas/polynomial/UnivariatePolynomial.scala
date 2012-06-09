package scas.polynomial

import scas.structure.EuclidianDomain
import scas.Implicits.{infixUFDOps, infixPowerProductOps}
import UnivariatePolynomial.Element

trait UnivariatePolynomial[S[C, N] <: Polynomial.Element[S[C, N], C, N], T <: Element[S, T, C, N], C, @specialized(Int, Long) N] extends PolynomialOverField[T, C, N] with PolynomialWithSyzygy[S, T, C, N] with EuclidianDomain[T] {
  assert (length == 1)
  def norm(x: T) = java.math.BigInteger.valueOf(degree(x))
  def derivative(w: T) = map(w, (a, b) => (a / pp.generators(0), b * ring(pp.degree(a))))
  def gcd(x: T, y: T) = if (y.isZero) monic(x) else gcd(y, monic(reduce(x, y)))
  def modInverse(x: T, mod: T) = {
    val w = gcd(apply(x, 0), mod)
    assert (w.isOne)
    fromPolynomial(w.element(0))
  }
}

object UnivariatePolynomial {
  trait Element[S[C, N] <: Polynomial.Element[S[C, N], C, N], T <: Element[S, T, C, N], C, N] extends PolynomialOverUFD.Element[T, C, N] with PolynomialWithSyzygy.Element[S, T, C, N] { this: T =>
    val factory: UnivariatePolynomial[S, T, C, N]
  }
}
