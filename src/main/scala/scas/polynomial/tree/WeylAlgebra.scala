package scas.polynomial.tree

import scas.polynomial.ordering.Ordering
import scas.polynomial.{TreePolynomial, PowerProduct}
import scas.structure.Ring
import TreePolynomial.Element

class WeylAlgebra[C, @specialized(Int, Long) N](val ring: Ring[C], val pp: PowerProduct[N])(implicit val ordering: Ordering[N], val cm: ClassManifest[Element[C, N]]) extends scas.polynomial.WeylAlgebra[Element[C, N], C, N] with TreePolynomial[C, N]

object WeylAlgebra {
  def apply[C, @specialized(Int, Long) N](ring: Ring[C], pp: PowerProduct[N])(implicit ordering: Ordering[N], cm: ClassManifest[Element[C, N]]) = new WeylAlgebra(ring, pp)
}
