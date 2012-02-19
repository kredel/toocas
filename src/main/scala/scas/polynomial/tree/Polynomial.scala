package scas.polynomial.tree

import scala.collection.SortedMap
import scas.polynomial.{TreePolynomial, PowerProduct, Lexicographic}
import scas.structure.Ring
import scas.Variable

class Polynomial[C <: Ring[C], @specialized(Int, Long) N: Numeric: Manifest](val ring: C, val pp: PowerProduct[N]) extends TreePolynomial[Polynomial[C, N], C, N] {
  def this(ring: C, ss: Array[Variable]) = this(ring, new Lexicographic[N](ss))
  def this(ring: C, s: Variable) = this(ring, Array(s))
  def this(ring: C, s: Variable, ss: Variable*) = this(ring, Array(s) ++ ss)
  def this(ring: C, sss: Array[Array[Variable]]) = this(ring, for (ss <- sss ; s <- ss) yield s)
  type E = Element
  val cm = implicitly[ClassManifest[E]]
  class Element(val value: SortedMap[Array[N], ring.E]) extends super.Element
  def apply(value: SortedMap[Array[N], ring.E]): E = new Element(value)
}
