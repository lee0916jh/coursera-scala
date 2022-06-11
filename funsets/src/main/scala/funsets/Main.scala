package funsets

object Main extends App :

  import FunSets.*

  val s = FunSets.union(singletonSet(1), singletonSet(2))
  val s1 = FunSets.union(s, singletonSet(3))

  println(contains(s1, 0))
  println(contains(s1, 1))
  println(contains(s1, 2))
  println(contains(s1, 3))
  println(contains(s1, 4))
  println("--")
  println(contains(map(s1, _ + 1), 0))
  println(contains(map(s1, _ + 1), 1))
  println(contains(map(s1, _ + 1), 2))
  println(contains(map(s1, _ + 1), 3))
  println(contains(map(s1, _ + 1), 4))