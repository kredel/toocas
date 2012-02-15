package scas.structure

trait Structure { outer =>
  type E <: Element
  def fromInt(i: Int): E
  def random(numbits: Int)(implicit rnd: scala.util.Random): E
  def compare(x: E, y: E): Int
  trait Element extends Ordered[E] { this: E =>
    def compare(that: E) = outer.compare(this, that)
    def ><(that: E) = outer.compare(this, that) == 0
    def <>(that: E) = outer.compare(this, that) != 0
    override def toString = toString(0)
    def toString(precedence: Int): String
  }
}

object Structure {
  implicit def struct2ordering(struct: Structure) = new Ordering[struct.E] {
    def compare(x: struct.E, y: struct.E) = struct.compare(x, y)
  }
}
