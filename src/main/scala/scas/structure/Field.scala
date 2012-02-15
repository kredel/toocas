package scas.structure

import scas.{int2bigInteger, ZZ}

trait Field extends EuclidianDomain with NotQuiteGroup {
  type E <: Element
  override def gcd(x: E, y: E) = if (norm(x) < norm(y)) y else x
  override def lcm(x: E, y: E) = if (norm(x) > norm(y)) y else x
  def norm(x: E) = ZZ(signum(abs(x)))
  def inverse(x: E) = one / x
  trait Element extends super[EuclidianDomain].Element with super[NotQuiteGroup].Element { this: E =>
    override def isUnit = !isZero
    override def %  (that: E) = zero
    override def /% (that: E) = (this / that, this % that)
  }
}
