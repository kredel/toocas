package scas.polynomial.ordering

import scas.polynomial.PowerProduct
import scas.Variable

class Lexicographic[@specialized(Int, Long) N: Numeric: Manifest](val variables: Array[Variable]) extends PowerProduct[N] {
  def this(s: Variable) = this(Array(s))
  def this(s: Variable, ss: Variable*) = this(Array(s) ++ ss)
  def this(sss: Array[Array[Variable]]) = this(for (ss <- sss ; s <- ss) yield s)
  val m = implicitly[Manifest[N]]
  val nm = implicitly[Numeric[N]]
  import nm.mkOrderingOps
  def instance(variables: Array[Variable]) = new Lexicographic[N](variables)
  def compare(x: Array[N], y: Array[N]): Int = {
    val n = x.length - 1
    for (i <- 0 until n) {
      if (x(i) < y(i)) return -1
      else if (x(i) > y(i)) return 1
    }
    0
  }
}
