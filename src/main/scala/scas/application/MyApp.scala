package scas.application

import scas._

object MyApp extends App {

  pp
  polynomial
  mod
  variable

  def pp = {
    val m = new PowerProduct(Array('x))
    val Array(x) = m.generators
    assert(1|x)
    assert(x*x >< x↑2)

    val n = new PowerProduct((for (i <- 0 until 4) yield (for (j <- 0 until 2) yield Variable("a", i, j)).toArray).toArray)
    val a = n.generatorsBy(2)
    println(a.deep);
  }

  def polynomial = {
    val r = new Polynomial(BigInt, Array('x))
    val s = new Polynomial(r, Array('y))
    val a = BigInt(1)
    val b = r(a)
    import s.{zero, one, pow}
    assert(zero+a >< one)
    assert(pow(a, 2) >< 1)
    assert(a↑2 >< 1)
  }

  def mod = {
    val r = new ModInteger(7)
    val s = new Polynomial(r, Array('x))
    val a = s(4)
    assert(2 * a >< 1)
    assert(r.characteristic.intValue == 7)
    assert(s.characteristic.intValue == 7)
  }

  def variable = {
    val a: Array[Variable] = Array('a, 'b, 'c)
    println(a.deep);
    println(Variable("x", 0, 0))
  }
}
