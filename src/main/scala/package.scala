import scas.structure.{Monoid, Ring, UniqueFactorizationDomain}
import scas.polynomial.PowerProduct

package object scas {
  trait ExtraImplicits {
    implicit val ZZ = scas.base.BigInteger
    implicit val QQ = scas.base.Rational
  }
  object Implicits extends ExtraImplicits with Ordering.ExtraImplicits with Ring.ExtraImplicits with UniqueFactorizationDomain.ExtraImplicits with PowerProduct.ExtraImplicits with scas.module.Module.ExtraImplicits

  val BigInteger = Implicits.ZZ
  val frac = Implicits.QQ
  val ModInteger = scas.base.ModInteger
  val Lexicographic = scas.polynomial.ordering.Lexicographic
  val DegreeLexicographic = scas.polynomial.ordering.DegreeLexicographic
  val DegreeReverseLexicographic = scas.polynomial.ordering.DegreeReverseLexicographic
  val KthElimination = scas.polynomial.ordering.KthElimination
  val PowerProduct = scas.polynomial.PowerProduct
  val Polynomial = scas.polynomial.tree.Polynomial
  val SolvablePolynomial = scas.polynomial.tree.SolvablePolynomial
  val WeylAlgebra = scas.polynomial.tree.WeylAlgebra
  val RationalFunction = scas.polynomial.ufd.RationalFunction
  val UnivariatePolynomial = scas.polynomial.ufd.tree.UnivariatePolynomial
  val MultivariatePolynomial = scas.polynomial.ufd.tree.MultivariatePolynomial
  val PolynomialWithSimpleGCD = scas.polynomial.ufd.tree.PolynomialWithSimpleGCD
  val PolynomialWithPrimitiveGCD = scas.polynomial.ufd.tree.PolynomialWithPrimitiveGCD
  val PolynomialWithSubresGCD = scas.polynomial.ufd.tree.PolynomialWithSubresGCD
  val Module = scas.module.Module
  implicit def int2bigInteger(i: Int) = java.math.BigInteger.valueOf(i)
  implicit def long2bigInteger(l: Long) = java.math.BigInteger.valueOf(l)
  implicit def int2powerProduct[N: PowerProduct](i: Int) = implicitly[PowerProduct[N]].apply(i)
  def pow[T: Monoid](x: T, exp: java.math.BigInteger) = implicitly[Monoid[T]].pow(x, exp)
  implicit val ordering = Lexicographic[Int]
}
