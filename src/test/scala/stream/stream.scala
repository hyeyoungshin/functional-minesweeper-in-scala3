package stream

import org.scalatest.funsuite.AnyFunSuite

class StreamTest extends AnyFunSuite {
  def construct_nat_from(n: Int): Stream[Int] = {
    new Stream(n, () => construct_nat_from(n + 1))
  }
  
  val nat = construct_nat_from(1)
  // Stream(1, () => construct_nat_from(2))
  // Syream(1, )

  test("take 0 should return the empty list") {  
    assert(nat.take(0) == List())
  }

  test("take 5 should return a list of five numbers") {
    assert(nat.take(3) == List(1, 2, 3))
  }

  test("map add one to nat") {
    val result = nat.map(x => x + 1).take(3)
    assert(result == List(2, 3, 4))
  }

  test("filter and take exactly as many as filtered should work") {
    val result = nat.filter(x => x < 3)
    // assert(result.take(3) == List(1, 2)) // StackOverFlow
    assert(result.take(2) == List(1, 2))
  }

  test("group") {
    val result = nat.group(3).take(2)
    assert(result == List(List(1,2,3), List(4,5,6)))
  }
}
