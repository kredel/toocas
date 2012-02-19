package scas.polynomial

import scas.structure.Ring

trait Polynomial[S <: Polynomial[S, C, N], C <: Ring[C], @specialized(Int, Long) N] extends Ring[S] {
  val ring: C
  val pp: PowerProduct[N]
  type E <: Element
  implicit val nm: Numeric[N]
  implicit val cm: ClassManifest[E]
  import nm.{max, fromInt}
  import pp.length
  def generator(n: Int) = apply(pp.generator(n))
  def generators = (for (i <- 0 until length) yield generator(i)).toArray
  def generatorsBy(n: Int) = {
    val m = length/n
    (for (i <- 0 until m) yield (for (j <- 0 until n) yield generator(i * n + j)).toArray).toArray
  }
  def characteristic = ring.characteristic
  def apply(i: Int) = ring(i)
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
        val c = ring.abs(b)
        val t = {
          if (a isOne) c.toString(2)
          else if (c isOne) a.toString()
          else c.toString(2) + "*" + a.toString()
        }
        if (n == 0) s = (if (ring.signum(b) < 0) "-" else "") + t
        else s = s + (if (ring.signum(b) < 0) "-" else "+") + t
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
  def apply(value: pp.E): E

  def iterator(x: E): Iterator[Pair[pp.E, ring.E]]

  def headPowerProduct(x: E) = {
    val (a, b) = headTerm(x)
    a
  }

  def headCoefficient(x: E) = {
    val (a, b) = headTerm(x)
    b
  }

  def headTerm(x: E) = iterator(x).next

  def degree(x: E) = (fromInt(0) /: iterator(x)) { (l, r) =>
      val (a, b) = r
      max(l, pp.degree(a))
    }

  def multiply(w: E, x: pp.E, y: ring.E) = map(w, (a, b) => (a * x, b * y))

  def multiply(w: E, y: ring.E) = map(w, (a, b) => (a, b * y))

  def map(w: E, f: (pp.E, ring.E) => (pp.E, ring.E)): E
}
