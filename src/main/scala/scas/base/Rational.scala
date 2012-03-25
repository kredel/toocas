package scas.base

import scas.structure.Quotient
import scas.{int2bigInteger, long2bigInteger}
import scas.Implicits.{ZZ, infixRingOps}
import Quotient.Element

object Rational extends Quotient[java.math.BigInteger] {
  def reduce(n: java.math.BigInteger, d: java.math.BigInteger) = {
    val gcd = n.gcd(d) match { case gcd => if (d.signum() < 0) gcd.negate() else gcd }
    apply(n.divide(gcd), d.divide(gcd))
  }
  override def apply(l: Long) = apply(l, 1)
  override def random(numbits: Int)(implicit rnd: scala.util.Random) = {
    val n = new java.math.BigInteger(numbits, rnd.self)
    val d = new java.math.BigInteger(numbits, rnd.self)
    reduce(if (rnd.nextBoolean()) n.negate() else n, d.add(1))
  }
  override def compare(x: Element[java.math.BigInteger], y: Element[java.math.BigInteger]) = {
    val Element(a, b) = x
    val Element(c, d) = y
    ring.compare(a * d, c * b)
  }
  override def toCode(x: Element[java.math.BigInteger], precedence: Int) = {
    val Element(n, d) = x
    if (ring.isOne(d)) ring.toCode(n, precedence)
    else "frac(" + ring.toCode(n, 0) + ", " + ring.toCode(d, 0) + ")"
  }
  override def toString = "QQ"
}
