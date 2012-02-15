package scas.polynomial

import scas.structure.UniqueFactorizationDomain

trait PolynomialOverUFD[C <: UniqueFactorizationDomain, @specialized(Int, Long) N] extends Polynomial[C, N] with UniqueFactorizationDomain {
  type E <: Element
  def divideAndRemainder(x: E, y: E): (E, E) = {
    if (y isZero) throw new ArithmeticException("Polynomial divide by zero")
    else if (x isZero) (zero, zero)
    else {
      val (s, a) = headTerm(x)
      val (t, b) = headTerm(y)
      if (!(t | s) || !(b | a)) (zero, x) else {
        val c = multiply(apply(s / t), a / b)
        divideAndRemainder(x - c * y, y) match { case (q, r) => (c + q, r) }
      }
    }
  }
  def gcd(x: E, y: E) = {
    val (a, p) = contentAndPrimitivePart(x)
    val (b, q) = contentAndPrimitivePart(y)
    multiply(primitivePart(gcd1(p, q)), ring.gcd(a, b))
  }
  def gcd1(x: E, y: E): E
  def remainder(x: E, y: E): E = {
    if (x isZero) zero
    else {
      val (s, a) = headTerm(x)
      val (t, b) = headTerm(y)
      if (!(t | s)) x else {
        val gcd = ring.gcd(a, b)
        val (a0, b0) = (a / gcd, b / gcd)
        remainder(multiply(x, b0) - multiply(y, s / t, a0), y)
      }
    }
  }
  def content(x: E) = (ring.zero /: iterator(x)) { (l, r) =>
    val (a, b) = r
    ring.gcd(l, b) match { case gcd => if (ring.signum(b) < 0) -gcd else gcd }
  }
  def contentAndPrimitivePart(x: E) = {
    val c = content(x)
    if (c isZero) (ring.zero, zero) else (c, divide(x, c))
  }
  def primitivePart(x: E) = {
    val (c, p) = contentAndPrimitivePart(x)
    p
  }
  trait Element extends super[Polynomial].Element with super[UniqueFactorizationDomain].Element { this: E =>
    def /  (that: E) = {
      val (d, r) = this /% that
      d
    }
    def %  (that: E) = {
      val (d, r) = this /% that
      r
    }
    def /% (that: E) = divideAndRemainder(this, that)
  }
  def divide(w: E, y: ring.E) = map(w, (a, b) => (a, b / y))
}
