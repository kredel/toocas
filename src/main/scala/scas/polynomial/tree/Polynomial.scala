package scas.polynomial.tree

import scas.polynomial.{TreePolynomial, PowerProduct, Lexicographic}
import scas.structure.Ring
import scas.Variable

class Polynomial[C <: Ring, @specialized(Int, Long) N: Numeric: Manifest](val ring: C, val pp: PowerProduct[N]) extends TreePolynomial[C, N] {
  def this(ring: C, ss: Array[Variable]) = this(ring, new Lexicographic[N](ss))
  def this(ring: C, s: Variable) = this(ring, Array(s))
  def this(ring: C, s: Variable, ss: Variable*) = this(ring, Array(s) ++ ss)
  def this(ring: C, sss: Array[Array[Variable]]) = this(ring, for (ss <- sss ; s <- ss) yield s)
}
