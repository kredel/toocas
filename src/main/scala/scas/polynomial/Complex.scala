package scas.polynomial

import scas.int2bigInteger

class Complex[R <: PolynomialOverUFD.Element[R, C, N], C, @specialized(Int, Long) N](ring: UnivariatePolynomial[R, C, N]) extends AlgebraicNumber[R, C, N](ring) {
  val I = generators(0)
  update(one + pow(I, 2))
}
