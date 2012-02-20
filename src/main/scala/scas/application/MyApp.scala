package scas.application

import scas._

object MyApp extends App {

  pp
  polynomial
  solvablePolynomial
  univariatePolynomial
  gcdSimple
  gcdPrimitive
  gcdSubres
  gcdMultivariate
  mod
  modPolynomial
  modSolvablePolynomial
  rational
  variable

  def pp = {
    val m = new PowerProduct('x)
    import m.{pow, generators}
    val Array(x) = generators
    assert(1 | x)
    assert(x * x >< pow(x, 2))

    val n = new PowerProduct((for (i <- 0 until 4) yield (for (j <- 0 until 2) yield Variable("a", i, j)).toArray).toArray)
    val a = n.generatorsBy(2)
    println(a.deep);
  }

  def polynomial = {
    val r = new Polynomial(ZZ, 'x)
    val s = new Polynomial(r, 'y)
    val a = s.ring.ring(1)
    import s.{zero, one, pow}
    assert(zero + a >< one)
    assert(pow(a, 2) >< 1)
  }

  def solvablePolynomial = {
    val r = new WeylAlgebra(ZZ, 'a, 'x, 'b, 'y)
    val Array(a, x, b, y) = r.generators
    assert (b*a+y*x >< 2+a*b+x*y)
    println(r)
  }

  def univariatePolynomial = {
    val r = new UnivariatePolynomial(QQ, 'x)
    import r.{generator, gcd, monic, ring => frac}
    val x = generator(0)
    assert (monic(gcd((1+x)*(1+frac(1, 2)*x), (1+frac(1, 2)*x)*(1-x))) >< 2+x)
  }

  def gcdSimple = {
    val r = new PolynomialWithSimpleGCD(ZZ, 'x)
    import r.{generators, gcd}
    val Array(x) = generators
    assert (gcd(0, 0) >< 0)
    assert (gcd(x, 0) >< x)
    assert (gcd(1, x) >< 1)
    assert (gcd((1+x)*(1+x), (1+x)*(1-x)) >< 1+x)
  }

  def gcdPrimitive = {
    val r = new PolynomialWithPrimitiveGCD(ZZ, 'x)
    import r.{generators, gcd}
    val Array(x) = generators
    assert (gcd((1+x)*(1+x), (1+x)*(1-x)) >< 1+x)
  }

  def gcdSubres = {
    val r = new PolynomialWithSubresGCD(ZZ, 'x)
    import r.{generators, gcd}
    val Array(x) = generators
    assert (gcd((1+x)*(1+x), (1+x)*(1-x)) >< 1+x)
  }

  def gcdMultivariate = {
    val r = new PolynomialWithSimpleGCD(ZZ, 'x, 'y, 'z)
    import r.{generators, gcd}
    val Array(x, y, z) = generators
    assert (gcd(x*y, x*z) >< x)
  }

  def mod = {
    val r = new ModInteger(7)
    val s = new Polynomial(r, 'x)
    val a = s(4)
    assert(2 * a >< 1)
    assert(s.characteristic.intValue == 7)
  }

  def modPolynomial = {
    val s = new ModPolynomial(7, 'x)
    val a = s(4)
    assert(2 * a >< 1)
    assert(s.characteristic.intValue == 7)
  }

  def modSolvablePolynomial = {
    val r = new ModSolvablePolynomial(2, 'a, 'x, 'b, 'y)
    val Array(a, x, b, y) = r.generators
    assert (b*a+y*x >< a*b+x*y)
    println(r)
  }

  def rational = {
    val frac = QQ
    val a = frac(1, 2)
    assert(a >< frac(2, 4))
    assert(a * a >< frac(1, 4))
    assert(a * 2 >< 1)
  }

  def variable = {
    val a: Array[Variable] = Array('a, 'b, 'c)
    println(a.deep);
    println(Variable("x", 0, 0))
  }
}
