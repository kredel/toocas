import scas.structure.{Ring, UniqueFactorizationDomain, Field}

package object scas {
  val ZZ = new scas.base.BigInt
  val QQ = new scas.base.Rational
  type ModInteger = scas.base.ModInteger
  type PowerProduct = scas.polynomial.PowerProduct[Int]
  val Lexicographic = new scas.polynomial.ordering.Lexicographic[Int]
  val DegreeLexicographic = new scas.polynomial.ordering.DegreeLexicographic[Int]
  val DegRevLex = new scas.polynomial.ordering.DegRevLex[Int]
  type KthElimination = scas.polynomial.ordering.KthElimination[Int]
  type Polynomial[C <: Ring[C]] = scas.polynomial.tree.Polynomial[C, Int]
  type SolvablePolynomial[C <: Ring[C]] = scas.polynomial.tree.SolvablePolynomial[C, Int]
  type WeylAlgebra[C <: Ring[C]] = scas.polynomial.tree.WeylAlgebra[C, Int]
  type UnivariatePolynomial[C <: Field[C]] = scas.polynomial.ufd.tree.UnivariatePolynomial[C, Int]
  type PolynomialWithSimpleGCD[C <: UniqueFactorizationDomain[C]] = scas.polynomial.ufd.tree.PolynomialWithSimpleGCD[C, Int]
  type PolynomialWithPrimitiveGCD[C <: UniqueFactorizationDomain[C]] = scas.polynomial.ufd.tree.PolynomialWithPrimitiveGCD[C, Int]
  type PolynomialWithSubresGCD[C <: UniqueFactorizationDomain[C]] = scas.polynomial.ufd.tree.PolynomialWithSubresGCD[C, Int]
  type ModPolynomial = scas.polynomial.mod.tree.Polynomial[Int]
  type ModSolvablePolynomial = scas.polynomial.mod.tree.SolvablePolynomial[Int]
  implicit def int2bigInteger(i: Int) = java.math.BigInteger.valueOf(i)
  implicit def long2bigInteger(l: Long) = java.math.BigInteger.valueOf(l)
  type BigInteger = java.math.BigInteger
  implicit val ordering = Lexicographic
}
