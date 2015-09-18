package adacola.fpinscala.chapter2

import scala.annotation.tailrec

object Exercise2_1 {

  case class Mat(seq: IndexedSeq[IndexedSeq[Int]]) {
    if (seq.isEmpty) throw new IllegalArgumentException("0行は不正です")
    if (seq(0).isEmpty) throw new IllegalArgumentException("0列は不正です")
    if (seq.exists(_.length != columnCount)) throw new IllegalArgumentException("列数が一定ではありません")

    lazy val rowCount = seq.length
    lazy val columnCount = seq(0).length

    def apply(r: Int, c: Int): Int = seq(r)(c)

    def *(rhs: Mat): Mat = {
      if (columnCount != rhs.rowCount) throw new IllegalArgumentException("乗算不可能です")
      val n = columnCount - 1
      val resultSeq =
        IndexedSeq.tabulate(rowCount, rhs.columnCount) { (i, j) =>
          (0 to n).map(k => apply(i, k) * rhs(k, j)).sum
        }
      Mat(resultSeq)
    }
  }

  object Mat {
    def E(n: Int): Mat = {
      Mat(IndexedSeq.tabulate(n, n) { (i, j) => if (i == j) 1 else 0 })
    }
  }

  object Fib {
    private val A = Mat(IndexedSeq(IndexedSeq(1, 1), IndexedSeq(1, 0)))
    private val E = Mat.E(2)

    /**
     * フィボナッチ数を行列計算を使用してO(log N)で求めます。
     */
    def fibMat(n: Int) = {
      /*
      行列Aを((1, 1), (1, 0))とおくと、A^nの2行1列目の要素の値がフィボナッチ数列のn項目と一致する。
      A^2 * A^2 = A^4、A^4 * A^4 = A^8 となることを利用して計算量をO(log N)に抑えることができる。
      ただし個々の演算は行列の乗算なので遅いため、nが大きくないと有利にならない。
       */
      @tailrec
      def loop(n: Int, a: Mat, b: Mat): Int = n match {
        case 0 => b(1, 0)
        case _ =>
          val br = if ((n & 1) == 0) b else a * b
          val ar = a * a
          loop(n >> 1, ar, br)
      }

      loop(n, A, E)
    }
  }

}
