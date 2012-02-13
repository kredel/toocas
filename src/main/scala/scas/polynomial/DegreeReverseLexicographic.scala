package scas.polynomial

import scas.Variable

trait DegreeReverseLexicographic[@specialized(Int, Long) N] extends PowerProduct[N] {
  import n.mkOrderingOps

  def compare(x: Array[N], y: Array[N]): Int = {
    val n = x.length - 1
    if (x(n) < y(n)) return -1
    else if (x(n) > y(n)) return 1
    for (i <- 0 until n) {
      if (x(i) > y(i)) return -1
      else if (x(i) < y(i)) return 1
    }
    0
  }
}
