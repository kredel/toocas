package scas.polynomial

import scala.collection.SortedMap
import scas.structure.Ring

trait SolvablePolynomial[C <: Ring, @specialized(Int, Long) N] extends Polynomial[C, N] {
  import pp.{dependencyOnVariables, projection, divide, isOne, one0}
  object table {
    type Key = Pair[Int, Int]
    type Relation = Triple[Array[N], Array[N], E]
    var content = SortedMap.empty[Key, List[Relation]]
    def update(e: E, f: E, p: E): Unit = update(headPowerProduct(e), headPowerProduct(f), p)
    def update(e: Array[N], f: Array[N], p: E) = {
      val key = makeKey(e, f)
      val list = content.getOrElse(key, Nil)
      content = content.updated(key, insert(list, (e, f, p)))
    }
    def insert(list: List[Relation], relation: Relation): List[Relation] = list match {
      case head::tail if (factorOf(relation, head)) => head::insert(tail, relation)
      case _ => relation::list
    }
    def lookup(e: Array[N], f: Array[N]): Relation = {
      val key = makeKey(e, f)
      val list = content.getOrElse(key, Nil)
      list match {
        case Nil => (one0, one0, apply(pp.multiply(e, f)))
        case _ => {
          val (e0, f0, p0) = select(list, (e, f, zero))
          (divide(e, e0), divide(f, f0), p0)
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
      pp.factorOf(ex, ey) && pp.factorOf(fx, fy)
    }
    def makeKey(e: Array[N], f: Array[N]) = {
      val de = dependencyOnVariables(e)
      val df = dependencyOnVariables(f)
      (de(0), df(0))
    }
    override def toString = "[" + (for ((a, b) <- content) yield "[" + (for ((e, f, p) <- b) yield pp.toString(e) + "*" + pp.toString(f) + " = " + p).mkString(", ") + "]").mkString(", ")+ "]"
  }
  override def toString = super.toString + table.toString

  override def multiply(w: E, x: Array[N], y: ring.E) = (zero /: iterator(w)) { (l, r) =>
    val (a, b) = r
    val c = b * y
    if (c isZero) l else l + multiply(multiply(a, x), c)
  }

  def multiply(w: E, y: ring.E): E = map(w, (a, b) => (a, b * y))

  def multiply(e: Array[N], f: Array[N]) = {
    val ep = dependencyOnVariables(e)
    val fp = dependencyOnVariables(f)
    if (ep.length == 0 || fp.length == 0) apply(pp.multiply(e, f)) else {
      val el = ep(ep.length-1)
      val fl = fp(0)
      if (el <= fl) apply(pp.multiply(e, f)) else {
        val e2 = projection(e, el)
        val f2 = projection(f, fl)
        val e1 = divide(e, e2)
        val f1 = divide(f, f2)
        val (e3, f3, c3) = table.lookup(e2, f2)
        var cs = c3
        if (!isOne(f3)) {
          cs = cs * apply(f3)
          table.update(divide(e2, e3), f2, cs)
        }
        if (!isOne(e3)) {
          cs = apply(e3) * cs
          table.update(e2, f2, cs)
        }
        if (!isOne(f1)) cs = cs * apply(f1)
        if (!isOne(e1)) cs = apply(e1) * cs
        cs
      }
    }
  }
}
