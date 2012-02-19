package scas.polynomial.mod.tree

import scas.polynomial.mod.TreePolynomial
import scas.polynomial.ordering.Lexicographic
import scas.polynomial.PowerProduct
import scas.base.ModInteger
import scas.Variable

class Polynomial[@specialized(Int, Long) N: Numeric: Manifest](mod: java.math.BigInteger, val pp: PowerProduct[N]) extends TreePolynomial[Polynomial[N], N] {
  def this(mod: java.math.BigInteger, ss: Array[Variable]) = this(mod, new Lexicographic[N](ss))
  def this(mod: java.math.BigInteger, s: Variable) = this(mod, Array(s))
  def this(mod: java.math.BigInteger, s: Variable, ss: Variable*) = this(mod, Array(s) ++ ss)
  def this(mod: java.math.BigInteger, sss: Array[Array[Variable]]) = this(mod, for (ss <- sss ; s <- ss) yield s)
  val ring = new ModInteger(mod)
}
