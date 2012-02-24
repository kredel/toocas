import scas.structure.Monoid
import scas.polynomial.PowerProduct

package object scas {
  val Ring = scas.structure.Ring
  val UFD = scas.structure.UniqueFactorizationDomain
  val PowerProduct = scas.polynomial.PowerProduct
  implicit val ZZ = scas.base.BigInteger
  implicit val QQ = scas.base.Rational
  val BigInteger = ZZ
  val frac = QQ
  val ModInteger = scas.base.ModInteger
  val Variable = scas.polynomial.Variable
  val Lexicographic = scas.polynomial.ordering.Lexicographic
  val DegreeLexicographic = scas.polynomial.ordering.DegreeLexicographic
  val DegreeReverseLexicographic = scas.polynomial.ordering.DegreeReverseLexicographic
  val KthElimination = scas.polynomial.ordering.KthElimination
  val Polynomial = scas.polynomial.tree.Polynomial
  val SolvablePolynomial = scas.polynomial.tree.SolvablePolynomial
  val WeylAlgebra = scas.polynomial.tree.WeylAlgebra
  val UnivariatePolynomial = scas.polynomial.ufd.tree.UnivariatePolynomial
  val PolynomialWithSimpleGCD = scas.polynomial.ufd.tree.PolynomialWithSimpleGCD
  val PolynomialWithPrimitiveGCD = scas.polynomial.ufd.tree.PolynomialWithPrimitiveGCD
  val PolynomialWithSubresGCD = scas.polynomial.ufd.tree.PolynomialWithSubresGCD
  implicit def int2bigInteger(i: Int) = java.math.BigInteger.valueOf(i)
  implicit def long2bigInteger(l: Long) = java.math.BigInteger.valueOf(l)
  implicit def int2powerProduct[N: PowerProduct](i: Int) = implicitly[PowerProduct[N]].apply(i)
  def pow[T: Monoid](x: T, exp: java.math.BigInteger) = implicitly[Monoid[T]].pow(x, exp)
  implicit val ordering = Lexicographic[Int]
}
