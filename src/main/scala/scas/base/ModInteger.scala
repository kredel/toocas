package scas.base

import scas.structure.Ring
import scas.ZZ

class ModInteger(val mod: java.math.BigInteger) extends Ring[java.math.BigInteger] {
  def apply(x: java.math.BigInteger) = x.mod(mod)
  def apply(l: Long) = apply(java.math.BigInteger.valueOf(l))
  def random(numbits: Int)(implicit rnd: scala.util.Random) = apply(new java.math.BigInteger(numbits, rnd.self))
  def characteristic = mod
  def isUnit(x: java.math.BigInteger) = apply(x) isOne
  override def pow(x: java.math.BigInteger, exp: java.math.BigInteger) = x.modPow(exp, mod)
  override def signum(x: java.math.BigInteger) = apply(x).signum()
  def plus(x: java.math.BigInteger, y: java.math.BigInteger) = apply(x.add(y))
  def minus(x: java.math.BigInteger, y: java.math.BigInteger) = apply(x.subtract(y))
  def times(x: java.math.BigInteger, y: java.math.BigInteger) = apply(x.multiply(y))
  def compare(x: java.math.BigInteger, y: java.math.BigInteger) = apply(x).compareTo(apply(y))
  override def toCode(x: java.math.BigInteger, precedence: Int) = ZZ.toCode(apply(x), precedence)
  override def toString = ZZ.toString + "(" + mod + ")"
}

object ModInteger {
  def apply(mod: java.math.BigInteger) = new ModInteger(mod)
}
