package chapter2

object Chapter2Solutions {
  def fibList(n: Int): List[Int] = {
    def go(toAdd: Int, next: Int, turns: Int, acc: List[Int]): List[Int] = turns match {
      case a if turns < n => go(next, toAdd + next, a + 1, acc :+ toAdd)
      case _              => acc 
    }

    go(0, 1, 0, List())
  }

  def fib(n: Int): Int = {
    def go(x: Int, y: Int, turns: Int): Int = turns match {
      case a if turns < n => go(y, x + y, a + 1)
      case _              => x
    }

    go(0, 1, 0)
  }

  def isSorted[A](as: List[A], unordered: (A, A) => Boolean): Boolean = {
    def go(n: Int): Boolean = 
      if (n >= as.length - 1) true
      else if (unordered(as(n), as(n + 1))) false 
      else go(n + 1)

    go(0)
  }

  // used for passing in isSorted
  def checkIntOrder(x: Int, y: Int): Boolean = 
    x > y

  def partial1[A,B,C](a: A, f: (A, B) => C): B => C =
    b => f(a, b)

  def curry[A,B,C](f: (A, B) => C): A => (B => C) =
    a => b => f(a, b)

  def uncurry[A,B,C](f: A => B => C): (A, B) => C =
    (a, b) => f(a)(b)

  def compose[A,B,C](f: B => C, g: A => B): A => C =
    a => f(g(a))
}