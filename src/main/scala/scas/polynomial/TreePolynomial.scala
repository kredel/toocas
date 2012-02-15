package scas.polynomial

import scala.collection.SortedMap
import scas.structure.Ring

trait TreePolynomial[C <: Ring, @specialized(Int, Long) N] extends Polynomial[C, N] {
  type E <: Element
  def zero = apply(SortedMap.empty[Array[N], ring.E](pp.reverse))
  def signum(x: E): Int = {
    val it = iterator(x)
    if (!it.hasNext) return 0
    val (a, b) = it.next ; ring.signum(b)
  }
  trait Element extends super.Element { this: E =>
    val value: SortedMap[Array[N], ring.E]
    override def isZero = value isEmpty
    def +(that: E) = apply((this.value /: iterator(that)) { (l, r) =>
      val (a, b) = r
      val c = l.getOrElse(a, ring.zero) + b
      if (c isZero) l - a else l.updated(a, c)
    })
    def -(that: E) = apply((this.value /: iterator(that)) { (l, r) =>
      val (a, b) = r
      val c = l.getOrElse(a, ring.zero) - b
      if (c isZero) l - a else l.updated(a, c)
    })
  }
  def apply(value: ring.E) = apply(zero.value + (pp.one.value -> value))
  def apply(value: pp.E) = apply(zero.value + (value.value -> ring.one))
  def apply(value: SortedMap[Array[N], ring.E]): E

  def iterator(x: E) = x.value.iterator

  override def headPowerProduct(x: E) = pp(x.value.lastKey)

  def map(w: E, f: (pp.E, ring.E) => (pp.E, ring.E)) = apply((zero.value /: iterator(w)) { (l, r) =>
    val (a, b) = r
    val (m, c) = f(pp(a), b)
    if (c isZero) l else l.updated(m.value, c)
  })
}
