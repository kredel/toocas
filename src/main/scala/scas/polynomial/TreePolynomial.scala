package scas.polynomial

import scala.collection.SortedMap
import scas.structure.Ring

trait TreePolynomial[C <: Ring, @specialized(Int, Long) N] extends Polynomial[C, N] { outer =>
  type S = TreePolynomial[C, N]
  type E <: Element
  override def zero = apply(SortedMap.empty[Array[N], ring.E](pp.reverse))
  def signum(x: E): Int = {
    val it = iterator(x)
    if (!it.hasNext) return 0
    val (a, b) = it.next ; ring.signum(b)
  }
  def fromElement(e: S#E) = apply((zero.value /: e.iterator) { (l, r) =>
    val (a, b) = r
    val (m, c) = (pp.fromElement(a), /*ring.fromElement(b)*/ ring.zero)
    if (c isZero) l else l.updated(m.value, c)
  })
  trait Element extends super.Element { this: E =>
    val value: SortedMap[Array[N], ring.E]
    override def isZero = value isEmpty
    def +(that: E) = apply((this.value /: that.iterator) { (l, r) =>
      val (a, b) = r
      val c = l.getOrElse(a.value, ring.zero) + b
      if (c isZero) l - a.value else l.updated(a.value, c)
    })
    def -(that: E) = apply((this.value /: that.iterator) { (l, r) =>
      val (a, b) = r
      val c = l.getOrElse(a.value, ring.zero) - b
      if (c isZero) l - a.value else l.updated(a.value, c)
    })
    def iterator = outer.iterator(this)
  }
  def apply(value: ring.E) = apply(zero.value + (pp.one.value -> value))
  def apply(value: pp.E) = apply(zero.value + (value.value -> ring.one))
  def apply(value: SortedMap[Array[N], ring.E]): E

  def iterator(x: E) = new Iterator[Pair[pp.E, ring.E]] {
    val it = x.value.iterator
    def hasNext = it.hasNext
    def next = {
      val (a, b) = it.next
      (pp(a), b)
    }
  }

  override def headPowerProduct(x: E) = pp(x.value.lastKey)

  def map(w: E, f: (pp.E, ring.E) => (pp.E, ring.E)) = apply((zero.value /: iterator(w)) { (l, r) =>
    val (a, b) = r
    val (m, c) = f(a, b)
    if (c isZero) l else l.updated(m.value, c)
  })
}
