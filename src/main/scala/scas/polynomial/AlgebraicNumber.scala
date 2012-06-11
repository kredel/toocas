package scas.polynomial

import scas.Variable
import scas.structure.Field
import Residue.Element

class AlgebraicNumber[R <: PolynomialOverUFD.Element[R, C, N], C, @specialized(Int, Long) N](override val ring: UnivariatePolynomial[R, C, N]) extends Residue(ring) with Field[Element[R, C, N]] {
  def update(mod: Element[R, C, N]): Unit = update(lift(mod))
  def update(mod: R) = {
    // assert mod is irreducible
    list = List(mod)
  }
  def mod = list(0)
  override def reduce(value: R) = apply(if (list.isEmpty) value else ring.reduce(value, mod))
  def inverse(x: Element[R, C, N]) = apply(ring.modInverse(lift(x), mod))
}

object AlgebraicNumber {
  def apply[C](ring: Field[C], s: Variable): AlgebraicNumber[tree.UnivariatePolynomial.Element[C, Int], C, Int] = apply(tree.UnivariatePolynomial(ring, s))
  def apply[R <: PolynomialOverUFD.Element[R, C, N], C, @specialized(Int, Long) N](ring: UnivariatePolynomial[R, C, N]) = new AlgebraicNumber(ring)
}
