package scas.polynomial.tree

import scas.polynomial.PowerProduct
import scas.structure.Ring
import Polynomial.Element

class SolvablePolynomial[C, @specialized(Int, Long) N](ring: Ring[C], pp: PowerProduct[N])(implicit cm: ClassManifest[Element[C, N]]) extends Polynomial(ring, pp) with scas.polynomial.SolvablePolynomial[Element[C, N], C, N]
