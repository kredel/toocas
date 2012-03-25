package scas.structure

trait Module[T, R] extends AbelianGroup[T] {
  implicit val ring: Ring[R]
  def rtimes(x: T, y: R): T
  def ltimes(x: R, y: T): T
  trait Ops extends super.Ops {
    def *(rhs: R) = rtimes(lhs, rhs)
  }
  class ScalarOps(val lhs: R) {
    def *(rhs: T) = ltimes(lhs, rhs)
  }
  def scalarOps(lhs: R): ScalarOps = new ScalarOps(lhs)
  override implicit def mkOps(value: T): Ops = new Ops { val lhs = value }
}
