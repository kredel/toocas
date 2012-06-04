package scas.base

import scas.polynomial.tree.Complex
import scas.Implicits.QQ

object Complex extends Complex(QQ) {
  override def toString = "CC"
  override def toMathML = <complexes/>
}
