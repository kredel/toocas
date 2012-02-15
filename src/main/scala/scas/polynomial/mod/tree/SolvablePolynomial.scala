package scas.polynomial.mod.tree

import scas.polynomial.mod.TreePolynomial
import scas.polynomial.{PowerProduct, Lexicographic}
import scas.base.ModInteger
import scas.Variable

class SolvablePolynomial[@specialized(Int, Long) N: Numeric: Manifest](mod: java.math.BigInteger, val pp: PowerProduct[N]) extends scas.polynomial.SolvablePolynomial[ModInteger, N] with TreePolynomial[N] {
  def this(mod: java.math.BigInteger, ss: Array[Variable]) = this(mod, new Lexicographic[N](ss))
  def this(mod: java.math.BigInteger, s: Variable, ss: Variable*) = this(mod, Array(s) ++ ss)
  def this(mod: java.math.BigInteger, sss: Array[Array[Variable]]) = this(mod, for (ss <- sss ; s <- ss) yield s)
  val ring = new ModInteger(mod)
}
