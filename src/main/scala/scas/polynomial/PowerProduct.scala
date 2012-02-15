package scas.polynomial

import scas.structure.Monoid
import scas.Variable

trait PowerProduct[@specialized(Int, Long) N] extends Monoid { outer =>
  val variables: Array[Variable]
  implicit val m: Manifest[N]
  val n: Numeric[N]
  import n.{fromInt => fromInteger, mkOrderingOps, mkNumericOps, min, max}
  type E = Element
  def one = apply(one0)
  override def pow(x: E, exp: java.math.BigInteger) = {
    assert (exp.signum() >= 0)
    apply(pow(x.value, fromBigInteger(exp)))
  }
  def fromBigInteger(value: java.math.BigInteger) = {
    (fromInteger(0) /: value.toByteArray()) { (s, b) => s * fromInteger(0xff) + fromInteger(b) }
  }
  def generator(n: Int) = apply(generator0(n))
  def generators = (for (i <- 0 until variables.length) yield generator(i)).toArray
  def generatorsBy(n: Int) = {
    val m = variables.length/n
    (for (i <- 0 until m) yield (for (j <- 0 until n) yield generator(i * n + j)).toArray).toArray
  }
  def fromInt(i: Int) = i
  def random(numbits: Int)(implicit rnd: scala.util.Random) = one
  def degree(x: E): N = degree(x.value)
  def gcd(x: E, y: E): E = apply(gcd(x.value,y.value))
  def scm(x: E, y: E): E = apply(scm(x.value,y.value))
  def compare(x: E, y: E) = compare(x.value, y.value)
  class Element(val value: Array[N]) extends super.Element {
    def isUnit = this isOne
    override def isOne = outer.isOne(value)
    def *(that: E) = apply(multiply(this.value, that.value))
    def /(that: E) = apply(divide(this.value, that.value))
    def |(that: E) = factorOf(this.value, that.value)
    def toString(precedence: Int) = outer.toString(value)
  }
  object Element {
    implicit def int2powerProduct(value: Int): E = {
      assert (value == 1)
      one
    }
  }
  def dependencyOnVariables(x: E): Array[Int] = dependencyOnVariables(x.value)
  def projection(x: E, n: Int): E = apply(projection(x.value, n))
  override def toString = "["+variables.mkString(", ")+"]"
  def apply(value: Array[N]): E = new Element(value)

  def one0 = {
    val l = variables.length + 1
    new Array[N](l)
  }

  def generator0(n: Int) = {
    val l = variables.length + 1
    (for (i <- 0 until l) yield fromInteger(if (i == n || i == l - 1) 1 else 0)).toArray
  }

  def degree(x: Array[N]): N = x(x.length-1)

  def pow(x: Array[N], exp: N) = (for (i <- 0 until x.length) yield x(i) * exp).toArray

  def multiply(x: Array[N], y: Array[N]) = (for (i <- 0 until x.length) yield x(i) + y(i)).toArray

  def divide(x: Array[N], y: Array[N]) = (for (i <- 0 until x.length) yield {
    assert (x(i) >= y(i))
    x(i) - y(i)
  }).toArray

  def factorOf(x: Array[N], y: Array[N]): Boolean = {
    for (i <- 0 until x.length - 1) if (x(i) > y(i)) return false
    true
  }

  def gcd(x: Array[N], y: Array[N]): Array[N] = {
    val r = new Array[N](x.length)
    for (i <- 0 until r.length - 1) r(i) = min(x(i), y(i))
    r(r.length - 1) = (fromInteger(0) /: r) { (s, l) => s + l }
    r
  }

  def scm(x: Array[N], y: Array[N]): Array[N] = {
    val r = new Array[N](x.length)
    for (i <- 0 until r.length - 1) r(i) = max(x(i), y(i))
    r(r.length - 1) = (fromInteger(0) /: r) { (s, l) => s + l }
    r
  }

  def compare(x: Array[N], y: Array[N]): Int

  def toString(x: Array[N]) = {
    var s = "1"
    var first = true
    for (i <- 0 until x.length - 1) {
      val n = x(i)
      if (n > fromInteger(0)) {
        val t = if (n equiv fromInteger(1)) variables(i).toString else variables(i).toString + "↑"+ x(i)
        if (first) s = t else s = s + "*" + t
        first = false
      }
    }
    s
  }

  def isOne(x: Array[N]) = x(x.length-1) == 0

  def dependencyOnVariables(x: Array[N]) = (for (i <- 0 until x.length - 1 if (x(i) > fromInteger(0))) yield i).toArray

  def projection(x: Array[N], n: Int) = (for (i <- 0 until x.length) yield if (i == n || i == x.length - 1) x(n) else fromInteger(0)).toArray
}

object PowerProduct {
  implicit def pp2ordering[@specialized(Int, Long) N](pp: PowerProduct[N]) = new Ordering[Array[N]] {
    def compare(x: Array[N], y: Array[N]) = pp.compare(x, y)
  }
}
