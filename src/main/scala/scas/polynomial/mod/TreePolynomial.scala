package scas.polynomial.mod

import scala.collection.SortedMap
import scas.polynomial.Polynomial
import scas.base.ModInteger

trait TreePolynomial[@specialized(Int, Long) N] extends Polynomial[ModInteger, N] { outer =>
  type S = TreePolynomial[N]
  type E = Element
  val cm = implicitly[ClassManifest[E]]
  override def zero = apply(SortedMap.empty[Array[N], java.math.BigInteger](pp.reverse))
  def signum(x: E): Int = {
    val it = iterator(x)
    if (!it.hasNext) return 0
    val (a, b) = it.next ; ring.signum(b)
  }
  def fromElement(e: S#E) = apply((zero.value /: e.iterator) { (l, r) =>
    val (a, b) = r
    val (m, c) = (pp.fromElement(a), ring.fromElement(b))
    if (c isZero) l else l.updated(m.value, c.value)
  })
  class Element(val value: SortedMap[Array[N], java.math.BigInteger]) extends super.Element {
    override def isZero = value isEmpty
    def +(that: E) = apply((this.value /: that.iterator) { (l, r) =>
      val (a, b) = r
      val c = ring(l.getOrElse(a.value, ring.zero.value)) + b
      if (c isZero) l - a.value else l.updated(a.value, c.value)
    })
    def -(that: E) = apply((this.value /: that.iterator) { (l, r) =>
      val (a, b) = r
      val c = ring(l.getOrElse(a.value, ring.zero.value)) - b
      if (c isZero) l - a.value else l.updated(a.value, c.value)
    })
    def iterator = outer.iterator(this)
  }
  def apply(value: ring.E) = apply(zero.value + (pp.one.value -> value.value))
  def apply(value: pp.E) = apply(zero.value + (value.value -> ring.one.value))
  def apply(value: SortedMap[Array[N], java.math.BigInteger]): E = new Element(value)

  def iterator(x: E) = new Iterator[Pair[pp.E, ring.E]] {
    val it = x.value.iterator
    def hasNext = it.hasNext
    def next = {
      val (a, b) = it.next
      (pp(a), ring(b))
    }
  }

  override def headPowerProduct(x: E) = pp(x.value.lastKey)

  def map(w: E, f: (pp.E, ring.E) => (pp.E, ring.E)) = apply((zero.value /: iterator(w)) { (l, r) =>
    val (a, b) = r
    val (m, c) = f(a, b)
    if (c isZero) l else l.updated(m.value, c.value)
  })
}
