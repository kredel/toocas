package scas.polynomial.ordering

import scas.Variable

class KthElimination[@specialized(Int, Long) N: Numeric: Manifest](val k: Int, val variables: Array[Variable]) extends DegreeReverseLexicographic[N] {
  def this(k: Int, s: Variable) = this(k, Array(s))
  def this(k: Int, s: Variable, ss: Variable*) = this(k, Array(s) ++ ss)
  def this(k: Int, sss: Array[Array[Variable]]) = this(k, for (ss <- sss ; s <- ss) yield s)
  val m = implicitly[Manifest[N]]
  val nm = implicitly[Numeric[N]]
  import nm.mkOrderingOps
  assert(k <= length)
  def instance(variables: Array[Variable]) = new KthElimination[N](k, variables)
  override def compare(x: Array[N], y: Array[N]): Int = {
    for (i <- 0 until k) {
      if (x(i) < y(i)) return -1
      else if (x(i) > y(i)) return 1
    }
    super.compare(x, y)
  }
}
