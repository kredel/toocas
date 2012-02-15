package scas.polynomial

import scala.collection.SortedMap
import scas.structure.Ring

trait SolvablePolynomial[C <: Ring, @specialized(Int, Long) N] extends Polynomial[C, N] {
  import pp.{dependencyOnVariables, projection}
  type Key = Pair[Int, Int]
  type Relation = Triple[pp.E, pp.E, E]
  var table = SortedMap.empty[Key, List[Relation]]
  def update(e: E, f: E, p: E): Unit = update(headPowerProduct(e), headPowerProduct(f), p)
  def update(e: pp.E, f: pp.E, p: E) = {
    val key = makeKey(e, f)
    val list = table.getOrElse(key, Nil)
    table = table.updated(key, insert(list, (e, f, p)))
  }
  def insert(list: List[Relation], relation: Relation): List[Relation] = list match {
    case head::tail if (factorOf(relation, head)) => head::insert(tail, relation)
    case _ => relation::list
  }
  def lookup(e: pp.E, f: pp.E): Relation = {
    val key = makeKey(e, f)
    val list = table.getOrElse(key, Nil)
    list match {
      case Nil => (pp.one, pp.one, apply(e * f))
      case _ => {
        val (e0, f0, p0) = select(list, (e, f, zero))
        (e / e0, f / f0, p0)
      }
    }
  }
  def select(list: List[Relation], relation: Relation): Relation = list match {
    case head::tail => if (factorOf(head, relation)) head else select(tail, relation)
    case _ => relation
  }
  def factorOf(x: Relation, y: Relation) = {
    val (ex, fx, px) = x
    val (ey, fy, py) = y
    (ex | ey) && (fx | fy)
  }
  def makeKey(e: pp.E, f: pp.E) = {
    val de = dependencyOnVariables(e)
    val df = dependencyOnVariables(f)
    (de(0), df(0))
  }
  override def toString = super.toString + "[" + (for ((a, b) <- table) yield "[" + (for ((e, f, p) <- b) yield e.toString() + "*" + f.toString() + " = " + p).mkString(", ") + "]").mkString(", ")+ "]"

  override def multiply(w: E, x: pp.E, y: ring.E) = (zero /: iterator(w)) { (l, r) =>
    val (a, b) = r
    val c = b * y
    if (c isZero) l else l + multiply(multiply(a, x), c)
  }

  def multiply(e: pp.E, f: pp.E) = {
    val ep = dependencyOnVariables(e)
    val fp = dependencyOnVariables(f)
    if (ep.length == 0 || fp.length == 0) apply(e * f) else {
      val el = ep(ep.length-1)
      val fl = fp(0)
      if (el <= fl) apply(e * f) else {
        val e2 = projection(e, el)
        val f2 = projection(f, fl)
        val e1 = e / e2
        val f1 = f / f2
        val (e3, f3, c3) = lookup(e2, f2)
        var cs = c3
        if (!(f3 isOne)) {
          cs = cs * apply(f3)
          update(e2 / e3, f2, cs)
        }
        if (!(e3 isOne)) {
          cs = apply(e3) * cs
          update(e2, f2, cs)
        }
        if (!(f1 isOne)) cs = cs * apply(f1)
        if (!(e1 isOne)) cs = apply(e1) * cs
        cs
      }
    }
  }
}
