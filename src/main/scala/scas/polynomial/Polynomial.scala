package scas.polynomial

import scas.structure.Ring

trait Polynomial[C <: Ring, @specialized(Int, Long) N] extends Ring {
  val ring: C
  val pp: PowerProduct[N]
  type E <: Element
  implicit val cm: ClassManifest[E]
  def one = apply(pp.one0)
  def generator(n: Int) = apply(pp.generator0(n))
  def generators = (for (i <- 0 until pp.variables.length) yield generator(i)).toArray
  def generatorsBy(n: Int) = {
     val m = pp.variables.length/n
    (for (i <- 0 until m) yield (for (j <- 0 until n) yield generator(i * n + j)).toArray).toArray
  }
  def characteristic = ring.characteristic
  def random(numbits: Int)(implicit rnd: scala.util.Random) = zero
  def compare(x: E, y: E): Int = {
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
  trait Element extends super.Element { this: E =>
    def isUnit = abs(this) isOne
    def *(that: E) = (zero /: iterator(that)) { (l, r) =>
      val (a, b) = r
      l + multiply(this, a, b)
    }
    def toString(precedence: Int) = {
      var s = ring.zero.toString(precedence)
      var n = 0
      for ((a, b) <- iterator(this)) {
        val c = abs(b)
        val t = {
          if (pp.isOne(a)) c.toString(2)
          else if (c isOne) pp.toString(a)
          else c.toString(2) + "*" + pp.toString(a)
        }
        if (n == 0) s = (if (signum(b) < 0) "-" + t else t)
        else s = (if (signum(b) < 0) s + "-" + t else s + "+" + t)
        n += 1
      }
      if ((if (n > 1) 1 else 2) < precedence) "(" + s + ")" else s
    }
  }
  object Element {
    implicit def coef2polynomial[D <% ring.E](value: D): E = apply(value)
  }
  override def toString = ring.toString + pp.toString
  def apply(value: ring.E): E
  def apply(value: Array[N]): E

  def iterator(x: E): Iterator[Pair[Array[N], ring.E]]

  def headPowerProduct(x: E) = headTerm(x)._1

  def headCoefficient(x: E) = headTerm(x)._2

  def headTerm(x: E) = iterator(x).next

  def degree(x: E): N = pp.degree(if (x isZero) pp.one0 else headPowerProduct(x))

  def multiply(w: E, x: Array[N], y: ring.E): E = map(w, (a, b) => (pp.multiply(a, x), b * y))

  def map(w: E, f: (Array[N], ring.E) => (Array[N], ring.E)): E
}
