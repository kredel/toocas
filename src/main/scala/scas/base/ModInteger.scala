package scas.base

import scas.structure.Residue
import scas.long2bigInteger
import scas.Implicits.ZZ

class ModInteger(val mod: java.math.BigInteger) extends Residue[java.math.BigInteger] {
  val ring = ZZ
  def apply(x: java.math.BigInteger) = x.mod(mod)
  override def apply(l: Long) = l
  override def random(numbits: Int)(implicit rnd: scala.util.Random) = apply(new java.math.BigInteger(numbits, rnd.self))
  def characteristic = mod
  override def pow(x: java.math.BigInteger, exp: java.math.BigInteger) = x.modPow(exp, mod)
  def compare(x: java.math.BigInteger, y: java.math.BigInteger) = ring.compare(x, y)
  override def toString = ring.toString + "(" + mod + ")"
}

object ModInteger {
  def apply(mod: java.math.BigInteger) = new ModInteger(mod)
}
