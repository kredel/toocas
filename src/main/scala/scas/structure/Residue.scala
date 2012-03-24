package scas.structure

trait Residue[R] extends Ring[R] {
  implicit val ring: Ring[R]
  def apply(x: R): R
  def apply(l: Long) = apply(ring(l))
  def random(numbits: Int)(implicit rnd: scala.util.Random) = apply(ring.random(numbits))
  def isUnit(x: R) = x.isOne
  override def pow(x: R, exp: java.math.BigInteger) = apply(ring.pow(x, exp))
  override def negate(x: R) = apply(ring.negate(x))
  override def abs(x: R) = x
  override def signum(x: R) = ring.signum(x)
  def plus(x: R, y: R) = apply(ring.plus(x, y))
  def minus(x: R, y: R) = apply(ring.minus(x, y))
  def times(x: R, y: R) = apply(ring.times(x, y))
  override def toCode(x: R, precedence: Int) = ring.toCode(x, precedence)
}
