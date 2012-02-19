package scas.base

import scas.structure.Field
import scas.{int2bigInteger, ZZ}

class Rational extends Field[Rational] {
  type E = Element
  override def pow(x: E, exp: java.math.BigInteger) = if (exp.signum() < 0) pow(inverse(x), exp.negate()) else apply(x.numerator.pow(exp.intValue()), x.denominator.pow(exp.intValue()))
  override def abs(x: E) = apply(x.numerator.abs(), x.denominator)
  def signum(x: E) = x.numerator.signum()
  def characteristic = 0
  def apply(i: Int) = i
  def apply(e: Rational#E) = apply(e.numerator, e.denominator)
  def random(numbits: Int)(implicit rnd: scala.util.Random) = {
    val n = new java.math.BigInteger(numbits, rnd.self)
    val d = new java.math.BigInteger(numbits, rnd.self)
    apply(if (rnd.nextBoolean()) n.negate() else n, d.add(1))
  }
  def compare(x: E, y: E) = x.numerator.multiply(y.denominator).compareTo(y.numerator.multiply(x.denominator))
  class Element(val numerator: java.math.BigInteger, val denominator: java.math.BigInteger) extends super.Element {
    def +(that: E) = apply(this.numerator.multiply(that.denominator).add(that.numerator.multiply(this.denominator)), this.denominator.multiply(that.denominator))
    def -(that: E) = apply(this.numerator.multiply(that.denominator).subtract(that.numerator.multiply(this.denominator)), this.denominator.multiply(that.denominator))
    def *(that: E) = apply(this.numerator.multiply(that.numerator), this.denominator.multiply(that.denominator))
    def /(that: E) = apply(this.numerator.multiply(that.denominator), this.denominator.multiply(that.numerator))
    def ÷(that: E) = this / that
    override def unary_- = apply(numerator.negate(), denominator)
    def toString(precedence: Int) = (numerator, denominator) match {
      case (n, d) => if (ZZ(d) isOne) ZZ(n).toString else {
        if (n.bitLength < 32 && d.bitLength < 32) "frac(" + n + ", " + d + ")"
        else if (n.bitLength < 64 && d.bitLength < 64) "frac(" + n + "l, " + d + "l)"
        else "frac(new BigInteger(\"" + n + "\"), new BigInteger(\"" + d + "\"))"
      }
    }
  }
  object Element {
    implicit def bigInteger2rational[D <% java.math.BigInteger](value: D): E = apply(value, 1)
  }
  override def toString = "QQ"
  def apply(numerator: java.math.BigInteger, denominator: java.math.BigInteger): E = {
    val gcd = numerator.gcd(denominator) match { case gcd => if (denominator.signum() < 0) gcd.negate() else gcd }
    new Element(numerator.divide(gcd), denominator.divide(gcd))
  }
}
