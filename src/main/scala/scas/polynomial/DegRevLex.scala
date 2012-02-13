package scas.polynomial

import scas.Variable

class DegRevLex[@specialized(Int, Long) N: Numeric: Manifest](val variables: Array[Variable]) extends DegreeReverseLexicographic[N] {
  def this(s: Variable) = this(Array(s))
  def this(s: Variable, ss: Variable*) = this(Array(s) ++ ss)
  def this(sss: Array[Array[Variable]]) = this(for (ss <- sss ; s <- ss) yield s)
  val m = implicitly[Manifest[N]]
  val n = implicitly[Numeric[N]]
}
