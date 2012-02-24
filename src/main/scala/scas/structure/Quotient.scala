package scas.structure

import Ring.Implicits.infixRingOps
import Quotient.Element

trait Quotient[R] extends Field[Element[R]] {
  implicit val ring: Ring[R]
  def apply(x: Element[R]) = {
    val Element(n, d) = x
    reduce(n, d)
  }
  def reduce(n: R, d: R): Element[R]
  def apply(n: R, d: R) = new Element(n, d)(this)
  def apply(n: R): Element[R] = apply(n, ring.one)
  def apply(l: Long) = apply(ring(l))
  def random(numbits: Int)(implicit rnd: scala.util.Random) = {
    val n = ring.random(numbits)
    val d = ring.random(numbits)
    reduce(if (rnd.nextBoolean()) -n else n, d + ring.one)
  }
  override def pow(x: Element[R], exp: java.math.BigInteger) = if (exp.signum() < 0) pow(inverse(x), exp.negate()) else {
    val Element(n, d) = x
    apply(ring.pow(n, exp), ring.pow(d, exp))
  }
  override def abs(x: Element[R]) = {
    val Element(n, d) = x
    apply(ring.abs(n), d)
  }
  def signum(x: Element[R]) = {
    val Element(n, d) = x
    ring.signum(n)
  }
  def characteristic = ring.characteristic
  def plus(x: Element[R], y: Element[R]) = {
    val Element(a, b) = x
    val Element(c, d) = y
    reduce(a * d + c * b, b * d)
  }
  def minus(x: Element[R], y: Element[R]) = {
    val Element(a, b) = x
    val Element(c, d) = y
    reduce(a * d - c * b, b * d)
  }
  def times(x: Element[R], y: Element[R]) = {
    val Element(a, b) = x
    val Element(c, d) = y
    reduce(a * c, b * d)
  }
  def divide(x: Element[R], y: Element[R]) = {
    val Element(a, b) = x
    val Element(c, d) = y
    reduce(a * d, b * c)
  }
  override def negate(x: Element[R]) = {
    val Element(n, d) = x
    apply(-n, d)
  }
}

object Quotient {
  case class Element[R](_1: R, _2: R)(override val factory: Quotient[R]) extends Product2[R, R] with UniqueFactorizationDomain.Element[Element[R]]
  implicit def ring2quotient[D, R](value: D)(implicit f: D => R, factory: Quotient[R]) = factory(value)
}
