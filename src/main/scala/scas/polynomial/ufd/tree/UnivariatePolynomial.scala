package scas.polynomial.ufd.tree

import scala.collection.SortedMap
import scas.polynomial.ordering.Ordering
import scas.polynomial.{TreePolynomial, PowerProduct}
import scas.structure.Field
import scas.Variable

class UnivariatePolynomial[C <: Field[C], @specialized(Int, Long) N](val ring: C, val pp: PowerProduct[N])(implicit val ordering: Ordering[N]) extends scas.polynomial.ufd.UnivariatePolynomial[UnivariatePolynomial[C, N], C, N] with TreePolynomial[UnivariatePolynomial[C, N], C, N] {
  def this(ring: C, s: Variable)(implicit nm: Numeric[N], m: Manifest[N], ordering: Ordering[N]) = this(ring, new PowerProduct[N](Array(s)))
  type E = Element
  val cm = implicitly[ClassManifest[E]]
  class Element(val value: SortedMap[Array[N], ring.E]) extends super[UnivariatePolynomial].Element with super[TreePolynomial].Element
  def apply(value: SortedMap[Array[N], ring.E]): E = new Element(value)
}
