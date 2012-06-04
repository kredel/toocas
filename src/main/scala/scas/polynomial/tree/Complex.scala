package scas.polynomial.tree

import scas.structure.Field

class Complex[C](ring: Field[C]) extends scas.polynomial.Complex[Polynomial.Element, UnivariatePolynomial.Element[C, Int], C, Int](UnivariatePolynomial(ring, "I"))
