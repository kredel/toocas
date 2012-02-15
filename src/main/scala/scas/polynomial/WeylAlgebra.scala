package scas.polynomial

import scas.structure.Ring

trait WeylAlgebra[C <: Ring, @specialized(Int, Long) N] extends SolvablePolynomial[C, N] {
  import pp.variables
  val n = variables.length >> 1
  for (i <- 0 until n; j = i + n) {
    val xi = pp.generator(i)
    val xj = pp.generator(j)
    update(xj, xi, apply(xi) * apply(xj) + one)
  }
}
