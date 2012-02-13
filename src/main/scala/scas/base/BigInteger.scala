package scas.base

import scas.structure.Ring
import scas.int2bigInteger

trait BigInteger extends Ring {
  type E = Element
  def zero = 0
  def one = 1
  override def pow(x: E, exp: java.math.BigInteger) = apply(x.value.pow(exp.intValue()))
  def signum(x: E) = x.value.signum()
  def characteristic = 0
  def random(numbits: Int)(implicit rnd: scala.util.Random) = zero
  def compare(x: E, y: E) = x.value.compareTo(y.value)
  class Element(val value: java.math.BigInteger) extends super.Element {
    def isUnit = abs(this) isOne
    def +(that: E) = apply(this.value.add(that.value))
    def -(that: E) = apply(this.value.subtract(that.value))
    def *(that: E) = apply(this.value.multiply(that.value))
    def toString(precedence: Int) = {
      if (value.bitLength < 32) value.toString
      else if (value.bitLength < 64) value + "l"
      else "new BigInteger(\"" + value + "\")"
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
