package scas.structure

trait AbelianGroup extends Structure {
  type E <: Element
  def zero: E
  def abs(x: E) = if (signum(x) < 0) -x else x
  def signum(x: E): Int
  trait Element extends super.Element { this: E =>
    def isZero = this >< zero
    def +(that: E): E
    def -(that: E): E
    def unary_+ = this
    def unary_- = zero - this
  }
}
