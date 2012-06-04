package scas.polynomial

import Residue.Element

class Residue[R <: PolynomialOverUFD.Element[R, C, N], C, @specialized(Int, Long) N](val ring: PolynomialOverField[R, C, N]) extends scas.structure.Residue[Element[R, C, N], R] {
  var list = List[R]()
  def generators = ring.generators.map(apply)
  def apply(value: C): Element[R, C, N] = apply(ring(value))
  def apply(value: R) = new Element[R, C, N](value)(this)
  def reduce(value: R) = apply(value)
  def lift(x: Element[R, C, N]) = x.value
  def characteristic = ring.characteristic
  override def toString = ring.ring.toString + "(" + list.mkString(", ") + ")"
  def toMathML = <msub>{ring.ring.toMathML}<mrow>{list.map(_.toMathML)}</mrow></msub>
}

object Residue {
  def apply[R <: PolynomialOverUFD.Element[R, C, N], C, @specialized(Int, Long) N](ring: PolynomialOverField[R, C, N]) = new Residue(ring)

  class Element[R <: PolynomialOverUFD.Element[R, C, N], C, @specialized(Int, Long) N](val value: R)(val factory: Residue[R, C, N]) extends scas.structure.Residue.Element[Element[R, C, N], R]
  object Element extends ExtraImplicits

  trait ExtraImplicits {
    implicit def coef2residue[D, R <: PolynomialOverUFD.Element[R, C, N], C, @specialized(Int, Long) N](value: D)(implicit f: D => C, factory: Residue[R, C, N]) = factory(value)
  }
}
