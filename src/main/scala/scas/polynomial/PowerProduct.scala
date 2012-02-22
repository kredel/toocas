package scas.polynomial

import scas.polynomial.ordering.Ordering
import scas.structure.Monoid
import PowerProduct.Element

class PowerProduct[@specialized(Int, Long) N](val variables: Array[Variable])(implicit nm: Numeric[N], m: Manifest[N], ordering: Ordering[N]) extends Monoid[Element[N]] {
  import scala.math.Ordering.Implicits.infixOrderingOps
  import Numeric.Implicits.infixNumericOps
  import nm.{fromInt, toLong}
  def take(n: Int) = new PowerProduct[N](variables.take(n))
  def drop(n: Int) = new PowerProduct[N](variables.drop(n))
  override def one = apply(one0)
  override def pow(x: Element[N], exp: java.math.BigInteger) = {
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
  def degree(x: Element[N]): Long = toLong(degree(x.value))
  def apply(l: Long) = {
    assert (l == 1)
    one
  }
  def apply(x: Element[N]) = apply(converter(x.factory.variables)(x.value))
  def random(numbits: Int)(implicit rnd: scala.util.Random) = one
  def gcd(x: Element[N], y: Element[N]): Element[N] = apply(gcd(x.value,y.value))
  def scm(x: Element[N], y: Element[N]): Element[N] = apply(scm(x.value,y.value))
  def compare(x: Element[N], y: Element[N]) = ordering.compare(x.value, y.value)
  def times(x: Element[N], y: Element[N]) = apply(multiply(x.value, y.value))
  def divide(x: Element[N], y: Element[N]): Element[N] = apply(divide(x.value, y.value))
  def factorOf(x: Element[N], y: Element[N]): Boolean = factorOf(x.value, y.value)
  def isUnit(x: Element[N]) = x isOne
  override def isOne(x: Element[N]) = isOne0(x.value)
  def dependencyOnVariables(x: Element[N]): Array[Int] = dependencyOnVariables(x.value)
  def projection(x: Element[N], n: Int): Element[N] = apply(projection(x.value, n))
  override def toCode(x: Element[N], precedence: Int) = toCode(x.value)
  override def toString = "["+variables.mkString(", ")+"]"
  def apply(value: Array[N]) = new Element(value)(this)

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
    for (i <- 0 until length) r(i) = nm.min(x(i), y(i))
    r(length) = (fromInt(0) /: r) { (s, l) => s + l }
    r
  }

  def scm(x: Array[N], y: Array[N]): Array[N] = {
    val r = new Array[N](length + 1)
    for (i <- 0 until length) r(i) = nm.max(x(i), y(i))
    r(length) = (fromInt(0) /: r) { (s, l) => s + l }
    r
  }

  def isOne0(x: Array[N]) = x(length) equiv fromInt(0)

  def dependencyOnVariables(x: Array[N]) = (for (i <- 0 until length if (x(i) > fromInt(0))) yield i).toArray

  def projection(x: Array[N], n: Int) = (for (i <- 0 until x.length) yield if (i == n || i == length) x(n) else fromInt(0)).toArray

  def toCode(x: Array[N]) = {
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

  def length = variables.length

  class Ops(val lhs: Element[N]) extends super.Ops {
    def /(rhs: Element[N]) = divide(lhs, rhs)
    def |(rhs: Element[N]) = factorOf(lhs, rhs)
  }
  override implicit def mkOps(lhs: Element[N]) = new Ops(lhs)
}

object PowerProduct {
  trait ExtraImplicits {
    implicit def infixPowerProductOps[N](lhs: Element[N])(implicit factory: PowerProduct[N]): PowerProduct[N]#Ops = factory.mkOps(lhs)
  }
  object Implicits extends ExtraImplicits

  def apply[@specialized(Int, Long) N](variables: Array[Variable])(implicit nm: Numeric[N], m: Manifest[N], ordering: Ordering[N]) = new PowerProduct[N](variables)
  def apply[@specialized(Int, Long) N](s: Variable)(implicit nm: Numeric[N], m: Manifest[N], ordering: Ordering[N]) = new PowerProduct[N](Array(s))
  def apply[@specialized(Int, Long) N](s: Variable, ss: Variable*)(implicit nm: Numeric[N], m: Manifest[N], ordering: Ordering[N]) = new PowerProduct[N](Array(s) ++ ss)
  def apply[@specialized(Int, Long) N](sss: Array[Array[Variable]])(implicit nm: Numeric[N], m: Manifest[N], ordering: Ordering[N]) = new PowerProduct[N](for (ss <- sss ; s <- ss) yield s)

  class Element[N](val value: Array[N])(override val factory: PowerProduct[N]) extends Monoid.Element[Element[N]] {
    def /(that: Element[N]) = factory.divide(this, that)
    def |(that: Element[N]) = factory.factorOf(this, that)
  }
  implicit def int2powerProduct[N](value: Int)(implicit factory: PowerProduct[N]) = factory(value)
}
