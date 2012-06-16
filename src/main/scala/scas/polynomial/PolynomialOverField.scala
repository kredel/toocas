package scas.polynomial

import scas.structure.Field
import scas.Implicits.{infixUFDOps, infixPowerProductOps}
import PolynomialOverUFD.Element

trait PolynomialOverField[T <: Element[T, C, N], C, @specialized(Int, Long) N] extends PolynomialOverUFD[T, C, N] {
  override implicit val ring: Field[C]
  override def divide(w: T, y: C) = multiply(w, ring.inverse(y))
  override def content(x: T) = if (x.isZero) ring.zero else headCoefficient(x)
  def monic(x: T) = primitivePart(x)
  override def reduce(x: T, y: T) = {
    if (x.isZero) zero
    else {
      val (s, a) = head(x)
      val (t, b) = head(y)
      if (!(t | s)) x else {
        reduce(x - multiply(y, s / t, a / b), y)
      }
    }
  }
}
