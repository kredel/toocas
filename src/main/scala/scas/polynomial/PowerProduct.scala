package scas.polynomial

import scas.polynomial.ordering.Ordering
import scas.structure.Monoid
import scas.Variable

class PowerProduct[@specialized(Int, Long) N](val variables: Array[Variable])(implicit nm: Numeric[N], m: Manifest[N], ordering: Ordering[N]) extends Monoid[PowerProduct[N]] { outer =>
  def this(s: Variable)(implicit nm: Numeric[N], m: Manifest[N], ordering: Ordering[N]) = this(Array(s))
  def this(s: Variable, ss: Variable*)(implicit nm: Numeric[N], m: Manifest[N], ordering: Ordering[N]) = this(Array(s) ++ ss)
  def this(sss: Array[Array[Variable]])(implicit nm: Numeric[N], m: Manifest[N], ordering: Ordering[N]) = this(for (ss <- sss ; s <- ss) yield s)
  import scala.math.Ordering.Implicits.infixOrderingOps
  import Numeric.Implicits.infixNumericOps
  import nm.{fromInt, min, max, toLong}
  type E = Element
  def take(n: Int) = new PowerProduct[N](variables.take(n))
  def drop(n: Int) = new PowerProduct[N](variables.drop(n))
  override def one = apply(one0)
  override def pow(x: E, exp: java.math.BigInteger) = {
    assert (exp.signum() >= 0)
    apply(pow(x.value, fromBigInteger(exp)))
  }
  def fromBigInteger(value: java.math.BigInteger) = {
    (fromInt(0) /: value.toByteArray()) { (s, b) => s * fromInt(0xff) + fromInt(b) }
  }
  def generator(n: Int) = apply(generator0(n))
  def generators = (for (i <- 0 until length) yield generator(i)).toArray
  def generatorsBy(n: Int) = {
    val m = length/n
    (for (i <- 0 until m) yield (for (j <- 0 until n) yield generator(i * n + j)).toArray).toArray
  }
  def degree(x: E): Long = toLong(degree(x.value))
  def apply(l: Long) = {
    assert (l == 1)
    one
  }
  def apply(e: PowerProduct[N]#E) = apply(converter(e.variables)(e.value))
  def random(numbits: Int)(implicit rnd: scala.util.Random) = one
  def gcd(x: E, y: E): E = apply(gcd(x.value,y.value))
  def scm(x: E, y: E): E = apply(scm(x.value,y.value))
  def compare(x: E, y: E) = ordering.compare(x.value, y.value)
  class Element(val value: Array[N]) extends super.Element {
    def isUnit = this isOne
    override def isOne = outer.isOne(value)
    def *(that: E) = apply(multiply(this.value, that.value))
    def /(that: E) = apply(divide(this.value, that.value))
    def |(that: E) = factorOf(this.value, that.value)
    def toString(precedence: Int) = outer.toString(value)
    def variables = outer.variables
  }
  object Element {
    implicit def int2powerProduct(value: Int): E = apply(value)
  }
  def dependencyOnVariables(x: E): Array[Int] = dependencyOnVariables(x.value)
  def projection(x: E, n: Int): E = apply(projection(x.value, n))
  override def toString = "["+variables.mkString(", ")+"]"
  def apply(value: Array[N]): E = new Element(value)

  def one0 = {
    new Array[N](length + 1)
  }

  def generator0(n: Int) = {
    (for (i <- 0 until length + 1) yield fromInt(if (i == n || i == length) 1 else 0)).toArray
  }

  def converter(from: Array[Variable]): Array[N] => Array[N] = { x =>
    val r = new Array[N](length + 1)
    val index = from map { a => variables.indexWhere(_ equiv a) }
    for (i <- 0 until from.length if (x(i) > fromInt(0))) {
      val c = index(i)
      assert (c > -1)
      r(c) = x(i)
    }
    r(length) = x(from.length)
    r
  }

  def degree(x: Array[N]): N = x(length)

  def pow(x: Array[N], exp: N) = (for (i <- 0 until x.length) yield x(i) * exp).toArray

  def multiply(x: Array[N], y: Array[N]) = (for (i <- 0 until x.length) yield x(i) + y(i)).toArray

  def divide(x: Array[N], y: Array[N]) = (for (i <- 0 until x.length) yield {
    assert (x(i) >= y(i))
    x(i) - y(i)
  }).toArray

  def factorOf(x: Array[N], y: Array[N]): Boolean = {
    for (i <- 0 until x.length) if (x(i) > y(i)) return false
    true
  }

  def gcd(x: Array[N], y: Array[N]): Array[N] = {
    val r = new Array[N](length + 1)
    for (i <- 0 until length) r(i) = min(x(i), y(i))
    r(length) = (fromInt(0) /: r) { (s, l) => s + l }
    r
  }

  def scm(x: Array[N], y: Array[N]): Array[N] = {
    val r = new Array[N](length + 1)
    for (i <- 0 until length) r(i) = max(x(i), y(i))
    r(length) = (fromInt(0) /: r) { (s, l) => s + l }
    r
  }

  def toString(x: Array[N]) = {
    var s = "1"
    var first = true
    for (i <- 0 until length) {
      val n = x(i)
      if (n > fromInt(0)) {
        val t = if (n equiv fromInt(1)) variables(i).toString else "pow(" + variables(i) + ", " + x(i) + ")"
        if (first) s = t else s = s + "*" + t
        first = false
      }
    }
    s
  }

  def isOne(x: Array[N]) = x(length) equiv fromInt(0)

  def dependencyOnVariables(x: Array[N]) = (for (i <- 0 until length if (x(i) > fromInt(0))) yield i).toArray

  def projection(x: Array[N], n: Int) = (for (i <- 0 until x.length) yield if (i == n || i == length) x(n) else fromInt(0)).toArray

  def length = variables.length
}
