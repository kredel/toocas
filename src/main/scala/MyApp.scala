object MyApp extends App {
  val r = new Poly(BigInt)
  val s = new Poly(r)
  val a = BigInt(1)
  val b = r(a)
  s.zero+a
}
