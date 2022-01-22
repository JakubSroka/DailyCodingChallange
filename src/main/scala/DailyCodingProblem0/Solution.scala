package DailyCodingProblem0

/*
  There's a staircase with N steps, and you can climb 1 or 2 steps at a time. Given N,
   write a function that returns the number of unique ways you can climb the staircase. The order of the steps matters.

  For example, if N is 4, then there are 5 unique ways:

  * 1, 1, 1, 1
  * 2, 1, 1
  * 1, 2, 1
  * 1, 1, 2
  * 2, 2

  N = 3

  1,1,1
  2,1
  1,2

  N = 5

  1,1,1,1,1
  2,1,1,1
  1,2,1,1
  1,1,2,1
  1,1,1,2
  2,2,1
  2,1,2
  1,2,2

 What if, instead of being able to climb 1 or 2 steps at a time, you could climb any number from a set of positive integers X?
 For example, if X = {1, 3, 5}, you could climb 1, 3, or 5 steps at a time. Generalize your function to take in X.
 */

object Solution extends  App {


  def getNumberOfUniqueWays(n: Int): Int = {
    if(n > 2) {
      getNumberOfUniqueWays(n-2) + getNumberOfUniqueWays(n-1)
    } else {
      n
    }
  }

  def getNumberOfUniqueWaysIt(n: Int): Int = {
    var a = 1
    var b = 2
    var tmp = 0
    for (_ <- 1 until n) {
      tmp = a
      a = b
      b = tmp + b
    }
    a
  }

//  println(getNumberOfUniqueWaysIt(1))
//  println(getNumberOfUniqueWaysIt(2))
//  println(getNumberOfUniqueWaysIt(3))
//  println(getNumberOfUniqueWaysIt(4))
//  println(getNumberOfUniqueWaysIt(5))

  def getNumberOfUniqWaysGeneric(X: Seq[Int], n: Int): Int  = {
    if (n < 0) {
      0
    } else if ( n ==0 ) {
      1
    } else {
      X.map(v => getNumberOfUniqWaysGeneric(X, n-v)).sum
    }
  }


//  // 1
//  println(getNumberOfUniqWaysGeneric(Seq(1,3,5), 1))
//  // 1 1
//  println(getNumberOfUniqWaysGeneric(Seq(1,3,5), 2))
//  // 1 1 1 | 3
//  println(getNumberOfUniqWaysGeneric(Seq(1,3,5), 3))
//  // 1 1 1 1 | 1 3 | 3 1
//  println(getNumberOfUniqWaysGeneric(Seq(1,3,5), 4))
//  // 1 1 1 1 1 | 1 1 3 | 1 3 1 | 3 1 1 | 5
//  println(getNumberOfUniqWaysGeneric(Seq(1,3,5), 5))
//  // 1 1 1 1 1 1 | 1 1 1 3 | 1 1 3 1 | 1 3 1 1 | 3 1 1 1 | 3 3 | 1 5 | 5 1
//  println(getNumberOfUniqWaysGeneric(Seq(1,3,5), 6))




  def getNumberOfUniqWaysGenericCached(X: Seq[Int], n: Int): Int  = {

    var cache: Map[Int, Int]= Map()

    def getWays(X: Seq[Int], n: Int) = {
      cache.getOrElse(n, compute(X, n))
    }

    def compute(X: Seq[Int], n: Int): Int = {
      if (n < 0) {
        0
      } else if ( n ==0 ) {
        1
      } else {
        val res = X.map(v => getWays(X, n-v)).sum
        cache = cache ++ Map(n -> res)
        res
      }
    }
    getWays(X, n)
  }

  // 1
  println(getNumberOfUniqWaysGenericCached(Seq(1,3,5), 1))
  // 1 1
  println(getNumberOfUniqWaysGenericCached(Seq(1,3,5), 2))
  // 1 1 1 | 3
  println(getNumberOfUniqWaysGenericCached(Seq(1,3,5), 3))
  // 1 1 1 1 | 1 3 | 3 1
  println(getNumberOfUniqWaysGenericCached(Seq(1,3,5), 4))
  // 1 1 1 1 1 | 1 1 3 | 1 3 1 | 3 1 1 | 5
  println(getNumberOfUniqWaysGenericCached(Seq(1,3,5), 5))
  // 1 1 1 1 1 1 | 1 1 1 3 | 1 1 3 1 | 1 3 1 1 | 3 1 1 1 | 3 3 | 1 5 | 5 1
  println(getNumberOfUniqWaysGenericCached(Seq(1,3,5), 6))

}
