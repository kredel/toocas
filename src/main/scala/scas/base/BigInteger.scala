package scas.base

import scas.structure.EuclidianDomain
import scas.{int2bigInteger, long2bigInteger}

trait BigInteger[S <: BigInteger[S]] extends EuclidianDomain[S] {
  type E = Element
  override def pow(x: E, exp: java.math.BigInteger) = apply(x.value.pow(exp.intValue()))
  def signum(x: E) = x.value.signum()
  def norm(x: E) = (abs(x) << 1) + (if (signum(x) < 0) 1 else 0)
  def gcd(x: E, y: E) = apply(x.value.gcd(y.value))
  def characteristic = 0
  def apply(l: Long) = l
  def apply(e: S#E) = apply(e.value)
  def random(numbits: Int)(implicit rnd: scala.util.Random) = {
    val r = new java.math.BigInteger(numbits, rnd.self)
    apply(if (rnd.nextBoolean()) r.negate() else r)
  }
  def compare(x: E, y: E) = x.value.compareTo(y.value)
  class Element(val value: java.math.BigInteger) extends super.Element {
    def isUnit = abs(this) isOne
    def ! : E = if (this > 1) this * (this - 1 !) else 1
    def +(that: E) = apply(this.value.add(that.value))
    def -(that: E) = apply(this.value.subtract(that.value))
    def *(that: E) = apply(this.value.multiply(that.value))
    def << (n: Int) = apply(this.value.shiftLeft(n))
    def /  (that: E) = apply(this.value.divide(that.value))
    def %  (that: E) = apply(this.value.remainder(that.value))
    def /% (that: E) = {
      val Array(d, r) = this.value.divideAndRemainder(that.value)
      (apply(d), apply(r))
    }
    def toString(precedence: Int) = value match {
      case n => if (n.bitLength < 32) n.toString
      else if (n.bitLength < 64) n + "l"
      else "new BigInteger(\"" + n + "\")"
    }
  }
  object Element {
    implicit def bigInteger2bigInteger[D <% java.math.BigInteger](value: D): E = apply(value)
  }
  override def toString = "ZZ"

  private val minCached = -1024
  private val maxCached = 1024
  private val cache = new Array[E](maxCached - minCached + 1)

  def apply(value: java.math.BigInteger): E = {
    val i = value.intValue()
    if (minCached <= i && i <= maxCached) {
      val offset = i - minCached
      var n = cache(offset)
      if (n eq null) { n = new Element(value); cache(offset) = n }
      n
    } else new Element(value)
  }
}
