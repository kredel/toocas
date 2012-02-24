package scas.base

import scas.structure.EuclidianDomain
import scas.{int2bigInteger, long2bigInteger}

object BigInteger extends EuclidianDomain[java.math.BigInteger] {
  def apply(x: java.math.BigInteger) = x
  def apply(s: String) = new java.math.BigInteger(s)
  def apply(l: Long) = l
  def random(numbits: Int)(implicit rnd: scala.util.Random) = {
    val r = new java.math.BigInteger(numbits, rnd.self)
    if (rnd.nextBoolean()) r.negate() else r
  }
  def characteristic = 0
  def isUnit(x: java.math.BigInteger) = abs(x) isOne
  override def pow(x: java.math.BigInteger, exp: java.math.BigInteger) = x.pow(exp.intValue())
  override def signum(x: java.math.BigInteger) = x.signum()
  def norm(x: java.math.BigInteger) = abs(x).shiftLeft(1).add(if (signum(x) < 0) 1 else 0)
  def gcd(x: java.math.BigInteger, y: java.math.BigInteger) = x.gcd(y)
  def divide(x: java.math.BigInteger, y: java.math.BigInteger) = x.divide(y)
  def remainder(x: java.math.BigInteger, y: java.math.BigInteger) = x.remainder(y)
  def divideAndRemainder(x: java.math.BigInteger, y: java.math.BigInteger) = {
    val Array(q, r) = x.divideAndRemainder(y)
    (q, r)
  }
  def plus(x: java.math.BigInteger, y: java.math.BigInteger) = x.add(y)
  def minus(x: java.math.BigInteger, y: java.math.BigInteger) = x.subtract(y)
  def times(x: java.math.BigInteger, y: java.math.BigInteger) = x.multiply(y)
  def compare(x: java.math.BigInteger, y: java.math.BigInteger) = x.compareTo(y)
  override def toCode(x: java.math.BigInteger, precedence: Int) = x match {
    case n => if (n.bitLength < 32) n.toString
    else if (n.bitLength < 64) n.toString + "l"
    else "BigInteger(\"" + n + "\")"
  }
  override def toString = "ZZ"
}
