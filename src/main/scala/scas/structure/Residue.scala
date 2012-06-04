package scas.structure

import scas.Implicits.infixUFDOps

trait Residue[T, R] extends UniqueFactorizationDomain[T] {
  implicit val ring: UniqueFactorizationDomain[R]
  def convert(x: T) = apply(ring.convert(lift(x)))
  def apply(l: Long) = apply(ring(l))
  def apply(value: R): T
  def reduce(value: R): T
  def lift(x: T): R
  def random(numbits: Int)(implicit rnd: java.util.Random) = reduce(ring.random(numbits))
  def isUnit(x: T) = lift(x).isUnit
  override def pow(x: T, exp: java.math.BigInteger) = reduce(ring.pow(lift(x), exp))
  override def negate(x: T) = reduce(-lift(x))
  override def abs(x: T) = reduce(ring.abs(lift(x)))
  override def signum(x: T) = ring.signum(lift(x))
  def plus(x: T, y: T) = reduce(lift(x) + lift(y))
  def minus(x: T, y: T) = reduce(lift(x) - lift(y))
  def times(x: T, y: T) = reduce(lift(x) * lift(y))
  def gcd(x: T, y: T) = apply(ring.gcd(lift(x), lift(y)))
  def divideAndRemainder(x: T, y: T) = {
    val (q, r) = lift(x) /% lift(y)
    (apply(q), apply(r))
  }
  def compare(x: T, y: T) = ring.compare(lift(x), lift(y))
  override def toCode(x: T, precedence: Int) = lift(x).toCode(precedence)
  def toMathML(x: T) = lift(x).toMathML
}

object Residue {
  trait Element[T <: Element[T, R], R] extends UniqueFactorizationDomain.Element[T] { this: T =>
    val factory: Residue[T, R]
    val value: R
  }
}
