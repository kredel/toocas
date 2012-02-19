package scas.polynomial.ufd.tree

import scala.collection.SortedMap
import scas.polynomial.ordering.Lexicographic
import scas.polynomial.{TreePolynomial, PowerProduct}
import scas.structure.UniqueFactorizationDomain
import scas.Variable

class PolynomialWithSubresGCD[C <: UniqueFactorizationDomain[C], @specialized(Int, Long) N: Numeric: Manifest](val ring: C, val pp: PowerProduct[N]) extends scas.polynomial.ufd.PolynomialWithSubresGCD[PolynomialWithSubresGCD, C, N] with TreePolynomial[PolynomialWithSubresGCD[C, N], C, N] {
  def this(ring: C, ss: Array[Variable]) = this(ring, new Lexicographic[N](ss))
  def this(ring: C, s: Variable) = this(ring, Array(s))
  def this(ring: C, s: Variable, ss: Variable*) = this(ring, Array(s) ++ ss)
  def this(ring: C, sss: Array[Array[Variable]]) = this(ring, for (ss <- sss ; s <- ss) yield s)
  type E = Element
  val nm = implicitly[Numeric[N]]
  val cm = implicitly[ClassManifest[E]]
  def split = new PolynomialWithSubresGCD(new PolynomialWithSubresGCD(ring, pp.take(location)), pp.drop(location))
  class Element(val value: SortedMap[Array[N], ring.E]) extends super[PolynomialWithSubresGCD].Element with super[TreePolynomial].Element
  def apply(value: SortedMap[Array[N], ring.E]): E = new Element(value)
}
