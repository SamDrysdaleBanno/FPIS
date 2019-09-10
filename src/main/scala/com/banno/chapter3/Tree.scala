package chapter3

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_)      => 1
    case Branch(x, y) => 1 + size(x) + size(y) 
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(x)      => x
    case Branch(x, y) => maximum(x) max maximum(y)
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_)      => 0
    case Branch(x, y) => 1 + (depth(x) max depth(y))
  }

  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(x)      => Leaf(f(x))
    case Branch(x, y) => Branch(map(x)(f), map(y)(f))
  }

  def fold[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B): B = t match {
    case Leaf(x) => f(x)
    case Branch(x, y) => g( fold(x)(f)(g), fold(y)(f)(g))
  }

  def size2[A](t: Tree[A]): Int =
    fold(t)(a => 1)(1 + _ + _)

  def maximum2(t: Tree[Int]): Int =
    fold(t)(a => a)(_ max _)
  
  def depth2[A](t: Tree[A]): Int =
    fold(t)(a => 0)((d1, d2) => 1 + (d1 max d2))

  def map2[A,B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_, _))
}