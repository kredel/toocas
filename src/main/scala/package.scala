import scas.structure.Ring

package object scas {
  val BigInt = scas.base.BigInt
  type ModInteger = scas.base.ModInteger
  type PowerProduct = scas.polynomial.PowerProduct[Int]
  type Polynomial[C <: Ring] = scas.polynomial.tree.Polynomial[C, Int]
  implicit def int2bigInteger(i: Int) = java.math.BigInteger.valueOf(i)
  implicit def long2bigInteger(l: Long) = java.math.BigInteger.valueOf(l)
  type BigInteger = java.math.BigInteger
}
