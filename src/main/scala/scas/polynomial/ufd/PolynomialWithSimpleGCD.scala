package scas.polynomial.ufd

import scas.Implicits.infixUFDOps
import PolynomialOverUFD.Element

trait PolynomialWithSimpleGCD[T[C, N] <: Element[T[C, N], C, N], C, N] extends MultivariatePolynomial[T, C, N] {
  def gcd1(x: T[C, N], y: T[C, N]) = if (y.isZero) x else gcd1(y, remainder1(x, y))
}
