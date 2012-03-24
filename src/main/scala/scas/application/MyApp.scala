package scas.application

import scas._

object MyApp extends App {

  pp1
  pp2
  polynomial
  solvablePolynomial
  bigint
  modint
  rational
  rationalPolynomial
  univariatePolynomial
  rf
  gcdSimple
  gcdPrimitive
  gcdSubres
  gcdMultivariate

  def pp1 = {
    import Implicits.{infixOrderingOps, infixPowerProductOps, int2powerProductOps}
    implicit val m = PowerProduct[Int]('x)
    val Array(x) = m.generators
    assert (x > 1)
    assert (1 | x)
    assert (x * x >< pow(x, 2))
  }

  def pp2 = {
    import Implicits.infixPowerProductOps
    implicit val m = PowerProduct[Int]((for (i <- 0 until 4) yield (for (j <- 0 until 2) yield Variable("a", i, j)).toArray).toArray)
    val a = m.generatorsBy(2)
    val s = (for (i <- 0 until 4) yield (for (j <- 0 until 2) yield a(i)(j).toCode(0)).toArray).toArray
    assert (s.deep.toString == "Array(Array(a(0)(0), a(0)(1)), Array(a(1)(0), a(1)(1)), Array(a(2)(0), a(2)(1)), Array(a(3)(0), a(3)(1)))");
  }

  def polynomial = {
    import Implicits.ZZ
    implicit val r = Polynomial(ZZ, PowerProduct[Int]('x))
    implicit val s = Polynomial(r, PowerProduct[Int]('y))
    val Array(x) = r.generators
    val Array(y) = s.generators
    assert (x + 1 >< 1 + x)
    assert (y + x >< x + y)
    assert (y + 1 >< 1 + y)
  }

  def solvablePolynomial = {
    import Implicits.ZZ
    implicit val r = WeylAlgebra(ZZ, PowerProduct[Int]('a, 'x, 'b, 'y))
    val Array(a, x, b, y) = r.generators
    assert (b*a+y*x >< 2+a*b+x*y)
    assert (r.toString == "ZZ[a, x, b, y][[b*a = a*b+1], [y*x = x*y+1]]")
  }

  def bigint = {
    import Implicits.{ZZ, infixRingOps, int2ringOps}
    val a = BigInteger(1)
    val b = a + a
    val c = pow(b, 32)
    val d = pow(c, 2)
    val e = BigInteger("18446744073709551616")
    assert (b >< 2)
    assert (2 >< b)
    assert (b.toCode(0) == "2")
    assert (c.toCode(0) == "4294967296l")
    assert (d.toCode(0) == "BigInteger(\"18446744073709551616\")")
    assert (d >< e)
  }

  def modint = {
    import Implicits.{infixRingOps, int2ringOps}
    implicit val r = ModInteger(7)
    val a = r(4)
    val b = a + a
    val c = pow(a, 2)
    assert (b >< 1)
    assert (1 >< b)
    assert (b.toString == "1")
    assert (c.toString == "2")
    assert (r.toString == "ZZ(7)")
    assert (r.characteristic.intValue == 7)
  }

  def rational = {
    import Implicits.QQ
    assert (1 + frac(1, 2) >< frac(1, 2) + 1)
    assert (frac(1, 2) + frac(3, 4) >< frac(5, 4))
  }

  def rationalPolynomial = {
    import Implicits.QQ
    implicit val r = Polynomial(QQ, PowerProduct[Int]('x))
    val Array(x) = r.generators
    assert (x + frac(1, 2) >< frac(1, 2) + x)
    assert (x + 1 >< 1 + x)
  }

  def univariatePolynomial = {
    import Implicits.QQ
    implicit val r = UnivariatePolynomial(QQ, PowerProduct[Int]('x))
    import r.{generator, gcd, monic}
    val x = generator(0)
    assert (monic(gcd((1+x)*(1+frac(1, 2)*x), (1+frac(1, 2)*x)*(1-x))) >< 2+x)
  }

  def rf = {
    import Implicits.QQ
    implicit val r = UnivariatePolynomial(QQ, PowerProduct[Int]('x))
    implicit val q = RationalFunction(r)
    val Array(x) = q.generators
    assert (x + frac(1, 2) >< frac(1, 2) + x)
    assert (x + 1 >< 1 + x)
    assert ((pow(x, 2) - 1)/(x - 1) >< x + 1)
    assert ((x/(2 * x)).toString == "frac(1, 2)")
    assert (q.toString == "QQ(x)")
  }

  def gcdSimple = {
    import Implicits.ZZ
    implicit val r = PolynomialWithSimpleGCD(ZZ, PowerProduct[Int]('x))
    import r.{generators, gcd}
    val Array(x) = generators
    assert (gcd(0, 0) >< 0)
    assert (gcd(x, 0) >< x)
    assert (gcd(1, x) >< 1)
    assert (gcd((1+x)*(1+x), (1+x)*(1-x)) >< 1+x)
  }

  def gcdPrimitive = {
    import Implicits.ZZ
    implicit val r = PolynomialWithPrimitiveGCD(ZZ, PowerProduct[Int]('x))
    import r.{generators, gcd}
    val Array(x) = generators
    assert (gcd((1+x)*(1+x), (1+x)*(1-x)) >< 1+x)
  }

  def gcdSubres = {
    import Implicits.ZZ
    implicit val r = PolynomialWithSubresGCD(ZZ, PowerProduct[Int]('x))
    import r.{generators, gcd}
    val Array(x) = generators
    assert (gcd((1+x)*(1+x), (1+x)*(1-x)) >< 1+x)
  }

  def gcdMultivariate = {
    import Implicits.ZZ
    implicit val r = MultivariatePolynomial(ZZ, PowerProduct[Int]('x, 'y, 'z))
    import r.{generators, gcd}
    val Array(x, y, z) = generators
    assert (gcd(x*y, x*z) >< x)
  }
}
