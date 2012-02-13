package scas.polynomial

import scas.structure.Ring

trait WeylAlgebra[C <: Ring, @specialized(Int, Long) N] extends SolvablePolynomial[C, N] {
  import pp.{generator0, variables}
  val n = variables.length >> 1
  for (i <- 0 until n; j = i + n) {
    val xi = generator0(i)
    val xj = generator0(j)
    table.update(xj, xi, apply(xi) * apply(xj) + one)
  }
}
