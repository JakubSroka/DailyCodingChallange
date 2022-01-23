package DailyCodingProblem1

/**
 * Given a list of numbers and a number k, return whether any two numbers from the list add up to k.

    For example, given [10, 15, 3, 7] and k of 17, return true since 10 + 7 is 17.
 */

object Solution extends App {

  def twoNumbersSumsUpTo(numbers: Seq[Int], k: Int): Boolean = {
    val set = collection.mutable.Set.empty[Int]
    numbers.foreach{ n =>
      if(set.contains(n)) return true
      set.add(k-n)
    }
    false
  }

  println(twoNumbersSumsUpTo(Seq(10,15,3,7), 17))
}
