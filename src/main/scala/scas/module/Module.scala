package scas.module

import scas.Variable
import scas.structure.Ring
import scas.Implicits.infixRingOps

class Module[R](val variables: Array[Variable])(implicit val ring: Ring[R], val cm: ClassManifest[R]) extends scas.structure.Module[Array[R], R] {
  def generator(n: Int) = (for (i <- 0 until length) yield if (i == n) ring.one else ring.zero).toArray
  def generators = (for (i <- 0 until length) yield generator(i)).toArray
  def apply(x: Array[R]) = (for (i <- 0 until length) yield if (i < x.length) ring(x(i)) else ring.zero).toArray
  def apply(l: Long) = apply(Array(ring(l)))
  override def random(numbits: Int)(implicit rnd: scala.util.Random) = (for (i <- 0 until length) yield ring.random(numbits)).toArray
  def compare(x: Array[R], y: Array[R]): Int = {
    for (i <- 0 until length) {
      val s = ring.compare(x(i), y(i))
      if (s < 0) return -1
      else if (s > 0) return 1
    }
    0
  }
  def plus(x: Array[R], y: Array[R]) = (for (i <- 0 until length) yield x(i) + y(i)).toArray
  def minus(x: Array[R], y: Array[R]) = (for (i <- 0 until length) yield x(i) - y(i)).toArray
  def rtimes(x: Array[R], y: R) = (for (i <- 0 until length) yield x(i) * y).toArray
  def ltimes(x: R, y: Array[R]) = (for (i <- 0 until length) yield x * y(i)).toArray
  override def toCode(x: Array[R], precedence: Int) = {
    var s = ""
    var n = 0
    var m = 0
    for (i <- 0 until length) {
      val c = ring.abs(x(i))
      val (t, u) = {
        if (c.isZero) ("", 0)
        else if (c.isOne) (variables(i), 1)
        else (c.toCode(1) + "*" + variables(i), 2)
      }
      if (u > 0) {
        s = s + (if (ring.signum(x(i)) < 0) "-" else (if (n == 0) "" else "+")) + t
        m = u + (if (ring.signum(x(i)) < 0) 1 else 0)
        n += 1
      }
    }
    if (n == 0) ring.zero.toCode(0) else {
      val fenced = {
        if (n == 1) {
          if (m == 1) false
          else precedence > 1
        } else precedence > 0
      }
      if (fenced) "(" + s + ")" else s
    }
  }
  override def toString = ring.toString + "^" + length

  def length = variables.length
}

object Module {
  trait ExtraImplicits {
    implicit def infixModuleOps[R: Module](lhs: Array[R]) = implicitly[Module[R]].mkOps(lhs)
    implicit def ring2moduleOps[S <% R, R: Module](lhs: S) = implicitly[Module[R]].scalarOps(lhs)
  }
  object Implicits extends ExtraImplicits

  def apply[R](name: String, dimension: Int, ring: Ring[R])(implicit cm: ClassManifest[R]) = new Module((for (i <- 0 until dimension) yield Variable(name, i)).toArray)(ring, cm)
}
