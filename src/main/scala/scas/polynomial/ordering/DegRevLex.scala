package scas.polynomial.ordering

class DegRevLex[@specialized(Int, Long) N](implicit val nm: scala.math.Ordering[N]) extends DegreeReverseLexicographic[N]
