package scas.base

import scas.polynomial.ufd.AlgebraicNumber
import scas.Implicits.{QQ, complex}
import scas.{int2bigInteger, pow}

object Complex extends AlgebraicNumber {
  val I = ring.generator(0)
  update(pow(I, 2) + 1)
}
