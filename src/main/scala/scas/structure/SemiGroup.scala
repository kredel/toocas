package scas.structure

trait SemiGroup extends Structure {
  type E <: Element
  trait Element extends super.Element { this: E =>
    def *(that: E): E
  }
}
