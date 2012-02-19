package scas.structure

trait EuclidianDomain[S <: EuclidianDomain[S]] extends UniqueFactorizationDomain[S] {
  def norm(x: E): Ordered[_]
}
