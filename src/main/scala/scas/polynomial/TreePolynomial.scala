package scas.polynomial

import scala.collection.SortedMap
import scas.structure.Ring

trait TreePolynomial[C <: Ring, @specialized(Int, Long) N] extends Polynomial[C, N] {
  import pp.one0
  type E = Element
  val cm = implicitly[ClassManifest[E]]
  def zero = apply(SortedMap.empty[Array[N], ring.E](pp.reverse))
  def signum(x: E): Int = {
    val it = iterator(x)
    if (!it.hasNext) return 0
    val (a, b) = it.next ; ring.signum(b)
  }
  class Element(val value: SortedMap[Array[N], ring.E]) extends super.Element {
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
  def apply(value: ring.E) = apply(zero.value + (one0 -> value))
  def apply(value: Array[N]) = apply(zero.value + (value -> ring.one))
  def apply(value: SortedMap[Array[N], ring.E]): E = new Element(value)

  def iterator(x: E) = x.value.iterator

  override def headPowerProduct(x: E) = x.value.lastKey

  def map(w: E, f: (Array[N], ring.E) => (Array[N], ring.E)) = apply((zero.value /: iterator(w)) { (l, r) =>
    val (a, b) = r
    val (m, c) = f(a, b)
    if (c isZero) l else l.updated(m, c)
  })
}
