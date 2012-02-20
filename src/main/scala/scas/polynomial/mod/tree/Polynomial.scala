package scas.polynomial.mod.tree

import scas.polynomial.mod.TreePolynomial
import scas.polynomial.ordering.Ordering
import scas.polynomial.PowerProduct
import scas.base.ModInteger
import scas.Variable

class Polynomial[@specialized(Int, Long) N](mod: java.math.BigInteger, val pp: PowerProduct[N])(implicit val ordering: Ordering[N]) extends TreePolynomial[Polynomial[N], N] {
  def this(mod: java.math.BigInteger, ss: Array[Variable])(implicit nm: Numeric[N], m: Manifest[N], ordering: Ordering[N]) = this(mod, new PowerProduct[N](ss))
  def this(mod: java.math.BigInteger, s: Variable)(implicit nm: Numeric[N], m: Manifest[N], ordering: Ordering[N]) = this(mod, Array(s))
  def this(mod: java.math.BigInteger, s: Variable, ss: Variable*)(implicit nm: Numeric[N], m: Manifest[N], ordering: Ordering[N]) = this(mod, Array(s) ++ ss)
  def this(mod: java.math.BigInteger, sss: Array[Array[Variable]])(implicit nm: Numeric[N], m: Manifest[N], ordering: Ordering[N]) = this(mod, for (ss <- sss ; s <- ss) yield s)
  val ring = new ModInteger(mod)
}
