package scas.base

import scas.structure.Field
import scas.Implicits.{ZZ, infixRingOps}

class PrimeModInteger(mod: java.math.BigInteger) extends ModInteger(mod) with Field[java.math.BigInteger] {
  assert (mod.isProbablePrime(100))
  def divide(x: java.math.BigInteger, y: java.math.BigInteger) = x * y.modInverse(mod)
}

object PrimeModInteger {
  def apply(mod: java.math.BigInteger) = new PrimeModInteger(mod)
}
