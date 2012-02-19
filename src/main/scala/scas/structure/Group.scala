package scas.structure

trait Group[S <: Group[S]] extends NotQuiteGroup[S] {
  type E <: Element
  trait Element extends super.Element { this: E =>
    override def isUnit = true
  }
}
