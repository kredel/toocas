import scas.structure.{Ring, Field}

package object scas {
  val ZZ = new scas.base.BigInt
  val QQ = new scas.base.Rational
  type ModInteger = scas.base.ModInteger
  type Lexicographic = scas.polynomial.Lexicographic[Int]
  type DegreeLexicographic = scas.polynomial.DegreeLexicographic[Int]
  type DegRevLex = scas.polynomial.DegRevLex[Int]
  type KthElimination = scas.polynomial.KthElimination[Int]
  type Polynomial[C <: Ring[C]] = scas.polynomial.tree.Polynomial[C, Int]
  type SolvablePolynomial[C <: Ring[C]] = scas.polynomial.tree.SolvablePolynomial[C, Int]
  type WeylAlgebra[C <: Ring[C]] = scas.polynomial.tree.WeylAlgebra[C, Int]
  type UnivariatePolynomial[C <: Field[C]] = scas.polynomial.tree.UnivariatePolynomial[C, Int]
  type ModPolynomial = scas.polynomial.mod.tree.Polynomial[Int]
  type ModSolvablePolynomial = scas.polynomial.mod.tree.SolvablePolynomial[Int]
  implicit def int2bigInteger(i: Int) = java.math.BigInteger.valueOf(i)
  implicit def long2bigInteger(l: Long) = java.math.BigInteger.valueOf(l)
  type BigInteger = java.math.BigInteger
}
