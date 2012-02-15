package scas.polynomial.tree

import scala.collection.SortedMap
import scas.polynomial.{TreePolynomial, PowerProduct, Lexicographic}
import scas.structure.Field
import scas.Variable

class UnivariatePolynomial[C <: Field, @specialized(Int, Long) N: Numeric: Manifest](val ring: C, val pp: PowerProduct[N]) extends scas.polynomial.UnivariatePolynomial[C, N] with TreePolynomial[C, N] {
  def this(ring: C, s: Variable) = this(ring, new Lexicographic[N](Array(s)))
  type E = Element
  val n = implicitly[Numeric[N]]
  val cm = implicitly[ClassManifest[E]]
  class Element(val value: SortedMap[Array[N], ring.E]) extends super[UnivariatePolynomial].Element with super[TreePolynomial].Element
  def apply(value: SortedMap[Array[N], ring.E]): E = new Element(value)
}
