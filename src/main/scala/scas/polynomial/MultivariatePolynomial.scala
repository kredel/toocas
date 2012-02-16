package scas.polynomial

import scas.structure.UniqueFactorizationDomain

import scas.int2bigInteger

trait MultivariatePolynomial[C <: UniqueFactorizationDomain, @specialized(Int, Long) N] extends PolynomialOverUFD[C, N] {
  import pp.{variables, projection}
  type F[C <: UniqueFactorizationDomain] <: MultivariatePolynomial[C, N]
  type S = F[C]
  def split: F[S]
  def location = variables.length - 1
  override def gcd(x: E, y: E) = if (variables.length > 1) {
    val s = split
    converter(s)(s.gcd(convert(s, x), convert(s, y)))
  } else super.gcd(x, y)
  def convert(s: F[S], w: E): s.E = (s.zero /: iterator(w)) { (l, r) =>
    val (a, b) = r
    val x = projection(a, location)
    l + s.multiply(s.pow(s.generator(0), n.toInt(pp.degree(x))), /*s.ring.fromElement(multiply(apply(a / x), b))*/ s.ring.one)
  }
  def converter(s: F[S])(w: s.E): E = (zero /: s.iterator(w)) { (l, r) =>
    val (a, b) = r
    l + fromElement(b) * pow(generator(location), n.toInt(s.pp.degree(a)))
  }
}
