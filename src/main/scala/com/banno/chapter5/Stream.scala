package com.banno.chapter5

import Stream._

sealed trait Stream[+A] {
  def toList: List[A] = this match {
    case Cons(h,t) => h() :: t().toList
    case _ => List()
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1  => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _                    => empty
  }

  def takeViaUnfold(n: Int): Stream[A] = 
    unfold( (this, n) ) {
      case (Cons(h, t), 1) => Some((h(), (empty, 0)))
      case (Cons(h, t), n) => Some((h(), (t(), n - 1)))
      case _               => None
    }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 1 => t().drop(n - 1)
    case _                   => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p)) 
    case Empty => empty
  }

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) if p(h()) => Some((h(), t()))
      case _                    => None
    }

  def exists(p: A => Boolean): Boolean = this match { 
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }  

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _          => z
  }

  def existsFold(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean = 
    foldRight(true)((a, b) => p(a) || b)
  
  def takeWhileFold(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) => 
      if (p(h)) cons(h, t)
      else empty
    )

  def headOption: Option[A] =
    foldRight(None: Option[A])((h,_) => Some(h))
  
  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((h, t) => cons(f(h), t))

  def mapViaUnfold[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(h,t) => Some((f(h()), t()))
      case _ => None
    }
  
  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) =>
      if (f(h)) cons(h, t)
      else t
    )

  def append[B>:A](s: => Stream[B]): Stream[B] =
    foldRight(s)((h, t) => cons(h, t))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h, t) => f(h).append(t))
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons( () => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = 
    f(z) match {
      case Some((h, s)) => cons(h, unfold(s)(f))
      case None         => empty
    }
  
  def constant[A](a: A): Stream[A] =
    cons(a, constant(a))

  def from(n: Int): Stream[Int] =
    cons(n, from(n + 1))

  val fibs = {
    def go(a: Int, b: Int): Stream[Int] =
      cons(a, go(b, a + b))
    go(0, 1)
  }

  val fibsViaUnfold = 
    unfold((0, 1)){ case (a, b) => Some((a, (b, a + b))) }

  def fromViaUnfold(n: Int) = 
    unfold(n)(n => Some((n, n + 1)))
  
  def constantViaUnfold[A](a: A) = 
    unfold(a)(_ => Some((a, a)))

  val onesViaUnfold =
    unfold(1)(_ => Some(1, 1))
}