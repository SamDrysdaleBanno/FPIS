package com.banno.funcats

import cats.Semigroup
import cats.implicits._

object CatsFun {
  val a = Semigroup[Int].combine(1, 2)
  val b = Semigroup[List[Int]].combine(List(1, 2, 3), List(4, 5, 6))
  val c = Semigroup[Option[Int]].combine(Option(1), Option(2))
  val d = Semigroup[Option[Int]].combine(Option(1), None)
  val extra = Map("foo" -> Map("bar" -> 5)) |+| Map("foo" -> Map("bar" -> 6), "baz" -> Map())

  def printAll: Unit = {
    println(s"a: $a")
    println(s"b: $b")
    println(s"c: $c")
    println(s"d: $d")
    println(s"extra: $extra")
  }
}