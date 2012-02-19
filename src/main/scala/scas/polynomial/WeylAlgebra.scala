package scas.polynomial

import scas.structure.Ring

trait WeylAlgebra[S <: WeylAlgebra[S, C, N], C <: Ring[C], @specialized(Int, Long) N] extends SolvablePolynomial[S, C, N] {
  import pp.variables
  val n = variables.length >> 1
  for (i <- 0 until n; j = i + n) {
    val xi = generator(i)
    val xj = generator(j)
    update(headPowerProduct(xj), headPowerProduct(xi), xi * xj + one)
  }
}
