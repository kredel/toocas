package scas.structure

trait Group extends NotQuiteGroup {
  type E <: Element
  trait Element extends super.Element { this: E =>
    override def isUnit = true
  }
}
