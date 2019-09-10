package chapter3

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil         => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](x: List[A]): List[A] = x match {
    case Nil        => sys.error("empty list")
    case Cons(_, t) => t
  }

  def setHead[A](toAdd: A, oldList: List[A]): List[A] = 
    Cons(toAdd, oldList)

  def drop[A](l: List[A], n: Int): List[A] = l match {
    case _ if n <= 0 => l
    case Nil => Nil
    case Cons(_, t) => drop(t, n - 1)
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case _ => l 
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h,t) => Cons(h, append(t, a2)) 
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil          => Nil
    case Cons(_, Nil) => Nil
    case Cons(h, t)   => Cons(h, init(t))
  }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil        => z
    case Cons(h, t) => f(h, foldRight(t, z)(f))
  }

  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil        => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Int]) = 
    foldRight(ns, 1)(_ * _)
  
  def length[A](as: List[A]): Int =
    foldRight(as, 0)((_, acc) => acc + 1)
  
  def testLength(): Int = {
    val testList = List(1,2,3)
    length(testList)
  }

  def sum3(ns: List[Int]): Int =
    foldLeft(ns, 0)(_ + _)

  def product3(ns: List[Int]): Int = 
    foldLeft(ns, 1)(_ * _)

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, List[A]())((acc, h) => Cons(h, acc))

  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] =
    foldRight(l, r)(Cons(_,_))

  def concat[A](l: List[List[A]]): List[A] = 
    foldRight(l, Nil:List[A])(append)

  def addOne(l: List[Int]): List[Int] =
    foldRight(l, Nil:List[Int])((h, t) => Cons(h + 1, t))

  def doubleToString(l: List[Double]): List[String] =
    foldRight(l, Nil:List[String])((h, t) => Cons(h.toString, t))

  def map[A,B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil:List[B])((h, t) => Cons(f(h), t))

  def filter[A](l: List[A])(f: A => Boolean): List[A] =
    foldRight(l, Nil: List[A])((h, t) => if (f(h)) Cons(h, t) else t)

  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] =
    foldRight(l, Nil:List[B])((h, t) => append(f(h), t))

  def filter2[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(a => if (f(a)) List(a) else Nil)

  def combineList(x: List[Int], y: List[Int]): List[Int] = (x, y) match {
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, combineList(t1, t2))
    case _                            => Nil
  }

  def zipWith[A,B,C](x: List[A], y: List[B])(f: (A, B) => C): List[C] = (x, y) match {
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
    case _                            => Nil
  }

  def startsWith[A](l: List[A], sup: List[A]): Boolean = (l, sup) match {
    case (_, Nil)                   => true
    case (Cons(h, t), Cons(h2, t2)) if (h == h2) => startsWith(t, t2)
    case _                          => false
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil                         => sub == Nil
    case _ if (startsWith(sup, sub)) => true
    case Cons(h, t)                  => hasSubsequence(t, sub)
  }
} 