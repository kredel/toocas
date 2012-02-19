package scas.polynomial.ufd

import scas.structure.UniqueFactorizationDomain

trait PolynomialWithPrimitiveGCD[S[C <: UniqueFactorizationDomain[C], N] <: PolynomialWithPrimitiveGCD[S, C, N], C <: UniqueFactorizationDomain[C], N] extends MultivariatePolynomial[S, C, N] { this: S[C, N] =>
  def gcd1(x: E, y: E) = if (y isZero) x else gcd1(y, primitivePart(remainder(x, y)))
}
