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

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def lengthFR[A](as: List[A]) = foldRight(as, 0) { (_, r) => r + 1 }

  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    @annotation.tailrec
    def loop(as: List[A], z: B): B = as match {
      case Nil => z
      case Cons(x, xs) => loop(xs, f(z, x))
    }
    loop(as, z)
  }

  def sumFL(as: List[Int]): Int = foldLeft(as, 0)(_ + _)

  def productFL(as: List[Double]): Double = foldLeft(as, 1.0)(_ * _)

  def lengthFL[A](as: List[A]): Int = foldLeft(as, 0) { (r, _) => r + 1 }

  def reverseFL[A](as: List[A]): List[A] = foldLeft(as, Nil: List[A]) { (xs, x) => Cons(x, xs) }

  def foldLeftFR[A, B](as: List[A], z: B)(f: (B, A) => B): B = foldRight(List.reverse(as), z) { (x, y) => f(y, x) }

  def foldRightFL[A, B](as: List[A], z: B)(f: (A, B) => B): B = foldLeft(List.reverse(as), z) { (x, y) => f(y, x) }

  def append[A](as: List[A], bs: List[A]): List[A] = foldRightFL(as, bs)(Cons(_, _))

  def flatten[A](ass: List[List[A]]): List[A] = foldRightFL(ass, Nil: List[A]) { (xs, ys) => append(xs, ys) }

  def add1(as: List[Int]): List[Int] = foldRightFL(as, Nil: List[Int]) { (x, xs) => Cons(x + 1, xs) }

  def doubleToString(as: List[Double]): List[String] = foldRightFL(as, Nil: List[String]) { (x, xs) => Cons(x.toString, xs) }

  def map[A, B](as: List[A])(f: A => B): List[B] = foldRightFL(as, Nil: List[B]) { (x, xs) => Cons(f(x), xs) }

}
