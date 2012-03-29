package scas.polynomial.ufd

import scas.structure.{Residue, Field}

class AlgebraicNumber[R <: UnivariatePolynomial.Element[R, C, N], C, N](override implicit val ring: UnivariatePolynomial[R, C, N]) extends Residue[R] with Field[R] {
  val rr = ring.ring
  var mod = ring.zero
  def update(mod: R) = {
    // assert mod is irreducible
    this.mod = mod
  }
  def apply(x: R) = if (mod.isZero) x else ring.remainder(x, mod)
  def characteristic = ring.characteristic
  def divide(x: R, y: R) = x * ring.modInverse(y, mod)
  override def toString = rr.toString + "(" + mod + ")"
}

object AlgebraicNumber {
  def apply[R <: UnivariatePolynomial.Element[R, C, N], C, N](implicit ring: UnivariatePolynomial[R, C, N]) = new AlgebraicNumber[R, C, N]
}
