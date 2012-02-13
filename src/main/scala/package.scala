import scas.structure.Ring

package object scas {
  val ZZ = scas.base.BigInt
  type ModInteger = scas.base.ModInteger
  type Lexicographic = scas.polynomial.Lexicographic[Int]
  type DegreeLexicographic = scas.polynomial.DegreeLexicographic[Int]
  type DegRevLex = scas.polynomial.DegRevLex[Int]
  type KthElimination = scas.polynomial.KthElimination[Int]
  type Polynomial[C <: Ring] = scas.polynomial.tree.Polynomial[C, Int]
  type SolvablePolynomial[C <: Ring] = scas.polynomial.tree.SolvablePolynomial[C, Int]
  type WeylAlgebra[C <: Ring] = scas.polynomial.tree.WeylAlgebra[C, Int]
  implicit def int2bigInteger(i: Int) = java.math.BigInteger.valueOf(i)
  implicit def long2bigInteger(l: Long) = java.math.BigInteger.valueOf(l)
  type BigInteger = java.math.BigInteger
}
