package adacola.fpinscala.datastructures

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](xs: List[A]): List[A] = xs match {
    case Nil => throw new IllegalArgumentException("xsが空です")
    case Cons(_, t) => t
  }

  def tryTail[A](xs: List[A]): Option[List[A]] = xs match {
    case Nil => None
    case Cons(_, t) => Some(t)
  }

  def setHead[A](xs: List[A], newHead: A): List[A] = Cons(newHead, tail(xs))

  def trySetHead[A](xs: List[A], newHead: A): Option[List[A]] = tryTail(xs).map(Cons(newHead, _))

  @annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] = n match {
    case 0 => l
    case _ => drop(tail(l), n - 1)
  }

  @annotation.tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(x, xs) if f(x) => dropWhile(xs, f)
    case _ => l
  }

  def reverse[A](l: List[A]) = {
    @annotation.tailrec
    def loop(l: List[A], r: List[A]): List[A] = l match {
      case Nil => r
      case Cons(x, xs) => loop(xs, Cons(x, r))
    }

    loop(l, Nil)
  }

  def init[A](l: List[A]): List[A] = reverse(tail(reverse(l)))

}
