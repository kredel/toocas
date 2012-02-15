package scas.structure

trait NotQuiteGroup extends Monoid {
  override def pow(x: E, exp: java.math.BigInteger) = if (exp.signum() < 0) pow(inverse(x), exp.negate()) else super.pow(x, exp)
  def inverse(x: E): E
}
