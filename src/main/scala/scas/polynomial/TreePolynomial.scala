package scas.polynomial

import scala.collection.SortedMap
import scas.structure.Ring
import Ring.Implicits.infixRingOps
import TreePolynomial.Element

trait TreePolynomial[C, N] extends Polynomial[Element[C, N], C, N] {
  override def zero = apply(SortedMap.empty[Array[N], C](ordering.reverse))
  override def signum(x: Element[C, N]): Int = {
    val it = iterator(x)
    if (!it.hasNext) return 0
    val (a, b) = it.next ; ring.signum(b)
  }
  def apply(x: Element[C, N]) = apply((zero.value /: x.value.iterator) { (l, r) =>
    val (a, b) = r
    val (m, c) = (pp.converter(x.factory.variables)(a), ring(b))
    if (c isZero) l else l.updated(m, c)
  })
  override def isZero(x: Element[C, N]) = x.value isEmpty
  def plus(x: Element[C, N], y: Element[C, N]) = apply((x.value /: y.value.iterator) { (l, r) =>
    val (a, b) = r
    val c = l.getOrElse(a, ring.zero) + b
    if (c isZero) l - a else l.updated(a, c)
  })
  def minus(x: Element[C, N], y: Element[C, N]) = apply((x.value /: y.value.iterator) { (l, r) =>
    val (a, b) = r
    val c = l.getOrElse(a, ring.zero) - b
    if (c isZero) l - a else l.updated(a, c)
  })
  def apply(value: C) = apply(if(value isZero) zero.value else zero.value + (pp.one.value -> value))
  def apply(value: PowerProduct.Element[N]) = apply(zero.value + (value.value -> ring.one))
  def apply(value: SortedMap[Array[N], C]) = new Element(value)(this)

  def iterator(x: Element[C, N]) = new Iterator[Pair[PowerProduct.Element[N], C]] {
    val it = x.value.iterator
    def hasNext = it.hasNext
    def next = {
      val (a, b) = it.next
      (pp(a), b)
    }
  }

  override def headPowerProduct(x: Element[C, N]) = pp(x.value.firstKey)

  def map(w: Element[C, N], f: (PowerProduct.Element[N], C) => (PowerProduct.Element[N], C)) = apply((zero.value /: w.value.iterator) { (l, r) =>
    val (a, b) = r
    val (m, c) = f(pp(a), b)
    if (c isZero) l else l.updated(m.value, c)
  })
}

object TreePolynomial {
  class Element[C, @specialized(Int, Long) N](val value: SortedMap[Array[N], C])(override val factory: TreePolynomial[C, N]) extends Polynomial.Element[Element[C, N], C, N]
  implicit def coef2polynomial[D, C, @specialized(Int, Long) N](value: D)(implicit f: D => C, factory: TreePolynomial[C, N]) = factory(value)
}
