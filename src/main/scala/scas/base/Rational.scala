package scas.base

import scas.structure.Field
import scas.{int2bigInteger, long2bigInteger, ZZ}

object Rational extends Field[(java.math.BigInteger, java.math.BigInteger)] {
  def apply(x: (java.math.BigInteger, java.math.BigInteger)) = {
    val (n, d) = x
    val gcd = n.gcd(d) match { case gcd => if (d.signum() < 0) gcd.negate() else gcd }
    (n.divide(gcd), d.divide(gcd))
  }
  def apply(l: Long) = (l, 1)
  def random(numbits: Int)(implicit rnd: scala.util.Random) = {
    val n = new java.math.BigInteger(numbits, rnd.self)
    val d = new java.math.BigInteger(numbits, rnd.self)
    apply(if (rnd.nextBoolean()) n.negate() else n, d.add(1))
  }
  override def pow(x: (java.math.BigInteger, java.math.BigInteger), exp: java.math.BigInteger) = if (exp.signum() < 0) pow(inverse(x), exp.negate()) else {
    val (n, d) = x
    apply(n.pow(exp.intValue()), d.pow(exp.intValue()))
  }
  override def abs(x: (java.math.BigInteger, java.math.BigInteger)) = {
    val (n, d) = x
    apply(n.abs(), d)
  }
  override def signum(x: (java.math.BigInteger, java.math.BigInteger)) = {
    val (n, d) = x
    n.signum()
  }
  def characteristic = 0
  def compare(x: (java.math.BigInteger, java.math.BigInteger), y: (java.math.BigInteger, java.math.BigInteger)) = {
    val (a, b) = x
    val (c, d) = y
    a.multiply(d).compareTo(c.multiply(b))
  }
  def plus(x: (java.math.BigInteger, java.math.BigInteger), y: (java.math.BigInteger, java.math.BigInteger)) = {
    val (a, b) = x
    val (c, d) = y
    apply(a.multiply(d).add(c.multiply(b)), b.multiply(d))
  }
  def minus(x: (java.math.BigInteger, java.math.BigInteger), y: (java.math.BigInteger, java.math.BigInteger)) = {
    val (a, b) = x
    val (c, d) = y
    apply(a.multiply(d).subtract(c.multiply(b)), b.multiply(d))
  }
  def times(x: (java.math.BigInteger, java.math.BigInteger), y: (java.math.BigInteger, java.math.BigInteger)) = {
    val (a, b) = x
    val (c, d) = y
    apply(a.multiply(c), b.multiply(d))
  }
  def divide(x: (java.math.BigInteger, java.math.BigInteger), y: (java.math.BigInteger, java.math.BigInteger)) = {
    val (a, b) = x
    val (c, d) = y
    apply(a.multiply(d), b.multiply(c))
  }
  override def negate(x: (java.math.BigInteger, java.math.BigInteger)) = {
    val (n, d) = x
    apply(n.negate(), d)
  }
  override def toCode(x: (java.math.BigInteger, java.math.BigInteger), precedence: Int) = {
    val (n, d) = apply(x)
    if (ZZ.isOne(d)) ZZ.toCode(n, precedence) else {
      if (n.bitLength < 32 && d.bitLength < 32) "frac(" + n + ", " + d + ")"
      else if (n.bitLength < 64 && d.bitLength < 64) "frac(" + n + "l, " + d + "l)"
      else "frac(new BigInteger(\"" + n + "\"), new BigInteger(\"" + d + "\"))"
    }
  }
  override def toString = "QQ"
}
