package scas.polynomial

import scas.structure.Field
import Residue.Element

trait AlgebraicNumber[R <: PolynomialOverUFD.Element[R, C, N], C, @specialized(Int, Long) N] extends Residue[R, C, N] with Field[Element[R, C, N]] {
  override val ring: UnivariatePolynomial[R, C, N]
  def update(mod: Element[R, C, N]): Unit = update(lift(mod))
  def update(mod: R) = {
    // assert mod is irreducible
    list = List(mod)
  }
  def mod = list(0)
  override def reduce(value: R) = apply(if (list.isEmpty) value else ring.reduce(value, mod))
  def inverse(x: Element[R, C, N]) = apply(ring.modInverse(lift(x), mod))
}
