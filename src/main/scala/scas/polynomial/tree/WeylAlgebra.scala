package scas.polynomial.tree

import scas.polynomial.PowerProduct
import scas.structure.Ring
import Polynomial.Element

class WeylAlgebra[C, @specialized(Int, Long) N](ring: Ring[C], pp: PowerProduct[N])(implicit cm: ClassManifest[Element[C, N]]) extends SolvablePolynomial(ring, pp) with scas.polynomial.WeylAlgebra[Element[C, N], C, N]
