package scas.structure

trait SemiGroup[S <: SemiGroup[S]] extends Structure[S] {
  type E <: Element
  trait Element extends super.Element { this: E =>
    def *(that: E): E
  }
}
