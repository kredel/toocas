package scas.base

import scas.structure.Ring

class ModInteger(val mod: java.math.BigInteger) extends BigInteger[ModInteger] {
  override def pow(x: E, exp: java.math.BigInteger): E = super.apply(x.value.modPow(exp, mod))
  override def characteristic = mod
  override def toString = super.toString + "(" + mod + ")"
  override def apply(value: java.math.BigInteger): E = super.apply(value.mod(mod))
}
