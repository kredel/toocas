package scas.polynomial

import scas.structure.UniqueFactorizationDomain

import scas.int2bigInteger

trait MultivariatePolynomial[S[C <: UniqueFactorizationDomain[C]] <: MultivariatePolynomial[S, C, N], C <: UniqueFactorizationDomain[C], N] extends PolynomialOverUFD[S[C], C, N] { this: S[C] =>
  import pp.{variables, projection}
  def split: S[S[C]]
  def location = variables.length - 1
  override def gcd(x: E, y: E) = if (variables.length > 1) {
    val s = split
    converter(s)(s.gcd(convert(s, x), convert(s, y)))
  } else super.gcd(x, y)
  def convert(s: S[S[C]], w: E): s.E = (s.zero /: iterator(w)) { (l, r) =>
    val (a, b) = r
    val x = projection(a, location)
    l + s.multiply(s.pow(s.generator(0), n.toInt(pp.degree(x))), s.ring(multiply(apply(a / x), b)))
  }
  def converter(s: S[S[C]])(w: s.E): E = (zero /: s.iterator(w)) { (l, r) =>
    val (a, b) = r
    l + apply(b) * pow(generator(location), n.toInt(s.pp.degree(a)))
  }
}
