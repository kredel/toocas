package scas.polynomial

import scas.polynomial.ordering.Ordering
import scas.structure.Ring
import Ring.Implicits.infixRingOps
import Polynomial.Element

trait Polynomial[T <: Element[T, C, N], C, N] extends Ring[T] {
  implicit val ring: Ring[C]
  val pp: PowerProduct[N]
  val ordering: Ordering[N]
  implicit val cm: ClassManifest[T]
  def generator(n: Int) = apply(pp.generator(n))
  def generators = (for (i <- 0 until pp.length) yield generator(i)).toArray
  def generatorsBy(n: Int) = {
    val m = pp.length/n
    (for (i <- 0 until m) yield (for (j <- 0 until n) yield generator(i * n + j)).toArray).toArray
  }
  def characteristic = ring.characteristic
  def apply(l: Long) = apply(ring(l))
  def random(numbits: Int)(implicit rnd: scala.util.Random) = zero
  def compare(x: T, y: T): Int = {
    val it = iterator(y)
    for ((a, b) <- iterator(x)) {
      if (!it.hasNext) return 1
      val (c, d) = it.next
      val s = pp.compare(a, c)
      if (s < 0) return -1
      else if (s > 0) return 1
      else {
        val s = ring.compare(b, d)
        if (s < 0) return -1
        else if (s > 0) return 1
      }
    }
    if (!it.hasNext) 0 else -1
  }
  def isUnit(x: T) = abs(x) isOne
  def times(x: T, y: T) = (zero /: iterator(y)) { (l, r) =>
    val (a, b) = r
    l + multiply(x, a, b)
  }
  override def toCode(x: T, precedence: Int) = {
    var s = ring.zero.toCode(0)
    var n = 0
    for ((a, b) <- iterator(x)) {
      val c = ring.abs(b)
      val t = {
        if (a isOne) c.toCode(0)
        else if (c isOne) a.toCode(0)
        else c.toCode(1) + "*" + a.toCode(1)
      }
      if (n == 0) s = (if (ring.signum(b) < 0) "-" else "") + t
      else s = s + (if (ring.signum(b) < 0) "-" else "+") + t
      n += 1
    }
    if (precedence > 0 && n > 1) "(" + s + ")" else s
  }
  override def toString = ring.toString + pp.toString
  def apply(value: C): T
  def apply(value: PowerProduct.Element[N]): T

  def iterator(x: T): Iterator[Pair[PowerProduct.Element[N], C]]

  def variables = pp.variables

  def headPowerProduct(x: T) = {
    val (a, b) = headTerm(x)
    a
  }

  def headCoefficient(x: T) = {
    val (a, b) = headTerm(x)
    b
  }

  def headTerm(x: T) = iterator(x).next

  def degree(x: T) = (0l /: iterator(x)) { (l, r) =>
      val (a, b) = r
      scala.math.max(l, pp.degree(a))
    }

  def multiply(w: T, x: PowerProduct.Element[N], y: C) = map(w, (a, b) => (a * x, b * y))

  def multiply(w: T, y: C) = map(w, (a, b) => (a, b * y))

  def map(w: T, f: (PowerProduct.Element[N], C) => (PowerProduct.Element[N], C)): T
}

object Polynomial {
  trait Element[T <: Element[T, C, N], C, N] extends Ring.Element[T] { this: T =>
  }
}
