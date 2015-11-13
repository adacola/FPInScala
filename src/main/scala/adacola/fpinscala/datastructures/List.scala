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

  // initは2回reverseが走って遅いのでこちらの実装の方がよい
  def init2[A](l: List[A]): List[A] = {
    @annotation.tailrec
    def loop(l: List[A], r: List[A]): List[A] = l match {
      case Nil => throw new IllegalArgumentException("lが空です")
      case Cons(_, Nil) => reverse(r)
      case Cons(x, xs) => loop(xs, Cons(x, r))
    }

    loop(l, Nil)
  }

  def foldRight[A, B](as: List[A], acc: B)(f: (A, B) => B): B = as match {
    case Nil => acc
    case Cons(x, xs) => f(x, foldRight(xs, acc)(f))
  }

  def sumFR(l: List[Int]): Int = foldRight(l, 0)(_ + _)

  def productFR(l: List[Int]): Int = foldRight(l, 1)(_ * _)

  def copy[A](l: List[A]) = foldRight(l, Nil: List[A])(Cons(_, _))

  def length[A](l: List[A]): Int = foldRight(l, 0)((_, r) => r + 1)

  @annotation.tailrec
  def foldLeft[A, B](as: List[A], acc: B)(f: (B, A) => B): B = as match {
    case Nil => acc
    case Cons(x, xs) => foldLeft(xs, f(acc, x))(f)
  }

  def reverseFL[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A])((xs, x) => Cons(x, xs))

  def foldRightFL[A, B](as: List[A], acc: B)(f: (A, B) => B): B = foldLeft(reverse(as), acc)((b, a) => f(a, b))

  def append[A](l1: List[A], l2: List[A]): List[A] = foldRightFL(l1, l2)(Cons(_, _))

  def flatten[A](ls: List[List[A]]): List[A] = foldRightFL(ls, Nil: List[A])(append)

  def map[A, B](l: List[A])(f: A => B): List[B] = foldRightFL(l, Nil: List[B])((a, bs) => Cons(f(a), bs))

  def add1(l: List[Int]): List[Int] = map(l)(_ + 1)

  def toString(l: List[Double]): List[String] = map(l)(_.toString)

  def filter[A](l: List[A])(f: A => Boolean): List[A] =
    foldRightFL(l, Nil: List[A])((a, bs) => if (f(a)) Cons(a, bs) else bs)

  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = foldRightFL(l, Nil: List[B])((a, b) => append(f(a), b))

  def filterFM[A](l: List[A])(f: A => Boolean): List[A] = flatMap(l)(a => if (f(a)) List(a) else Nil)

  def add(l1: List[Int], l2: List[Int]): List[Int] = {
    @annotation.tailrec
    def loop(l1: List[Int], l2: List[Int], acc: List[Int]): List[Int] = (l1, l2) match {
      case (Nil, _) | (_, Nil) => reverse(acc)
      case (Cons(x, xs), Cons(y, ys)) => loop(xs, ys, Cons(x + y, acc))
    }

    loop(l1, l2, Nil)
  }

  def zipWith[A, B, C](l1: List[A], l2: List[B])(f: (A, B) => C): List[C] = {
    @annotation.tailrec
    def loop(l1: List[A], l2: List[B], acc: List[C]): List[C] = (l1, l2) match {
      case (Nil, _) | (_, Nil) => reverse(acc)
      case (Cons(x, xs), Cons(y, ys)) => loop(xs, ys, Cons(f(x, y), acc))
    }

    loop(l1, l2, Nil)
  }

  def hasSubSequence[A](sup: List[A], sub: List[A]): Boolean = {
    
  }

}
