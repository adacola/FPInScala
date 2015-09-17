package adacola.fpinscala.chapter2

object Exercise2_2 {

  def isSorted[A](aArray: Array[A], aOrdered: (A, A) => Boolean): Boolean = {
    aArray.zip(aArray.drop(1)).forall { case (x, y) => aOrdered(x, y) }
  }

}
