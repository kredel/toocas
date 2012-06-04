package scas.polynomial

import scas.int2bigInteger

class Complex[S[C, N] <: Polynomial.Element[S[C, N], C, N], R <: UnivariatePolynomial.Element[S, R, C, N], C, @specialized(Int, Long) N](ring: UnivariatePolynomial[S, R, C, N]) extends AlgebraicNumber[S, R, C, N](ring) {
  val I = generators(0)
  update(one + pow(I, 2))
}
