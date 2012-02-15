package scas.structure

trait EuclidianDomain extends UniqueFactorizationDomain {
  def norm(x: E): Ordered[_]
}
