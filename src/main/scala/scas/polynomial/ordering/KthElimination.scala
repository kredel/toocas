package scas.polynomial.ordering

class KthElimination[@specialized(Int, Long) N](val k: Int)(implicit val nm: scala.math.Ordering[N]) extends DegreeReverseLexicographic[N] {
  import scala.math.Ordering.Implicits.infixOrderingOps
  override def compare(x: Array[N], y: Array[N]): Int = {
    for (i <- 0 until k) {
      if (x(i) < y(i)) return -1
      else if (x(i) > y(i)) return 1
    }
    super.compare(x, y)
  }
}
