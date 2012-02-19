package scas.polynomial.tree

import scala.collection.SortedMap
import scas.polynomial.ordering.Lexicographic
import scas.polynomial.{TreePolynomial, PowerProduct}
import scas.structure.Field
import scas.Variable

class UnivariatePolynomial[C <: Field[C], @specialized(Int, Long) N: Numeric: Manifest](val ring: C, val pp: PowerProduct[N]) extends scas.polynomial.UnivariatePolynomial[UnivariatePolynomial[C, N], C, N] with TreePolynomial[UnivariatePolynomial[C, N], C, N] {
  def this(ring: C, s: Variable) = this(ring, new Lexicographic[N](Array(s)))
  type E = Element
  val n = implicitly[Numeric[N]]
  val cm = implicitly[ClassManifest[E]]
  class Element(val value: SortedMap[Array[N], ring.E]) extends super[UnivariatePolynomial].Element with super[TreePolynomial].Element
  def apply(value: SortedMap[Array[N], ring.E]): E = new Element(value)
}
