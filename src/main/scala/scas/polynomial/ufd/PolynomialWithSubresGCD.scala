package scas.polynomial.ufd

import scas.structure.UniqueFactorizationDomain
import scas.long2bigInteger

trait PolynomialWithSubresGCD[S[C <: UniqueFactorizationDomain[C], N] <: PolynomialWithSubresGCD[S, C, N], C <: UniqueFactorizationDomain[C], N] extends MultivariatePolynomial[S, C, N] { this: S[C, N] =>
  def gcd1(x: E, y: E) = if (degree(x) < degree(y)) gcd1(y, x) else gcd(x, y, ring.one, ring.one)
  def gcd(x: E, y: E, beta: ring.E, phi: ring.E): E = if (y isZero) x else if (x isZero) y else {
    val d = degree(x) - degree(y)
    gcd(y, divide(remainder(x, y), beta), headCoefficient(x) * ring.pow(phi, d), if (d == 0) phi else if (d == 1) headCoefficient(y) else ring.pow(headCoefficient(y), d) / ring.pow(phi, d - 1))
  }
}
