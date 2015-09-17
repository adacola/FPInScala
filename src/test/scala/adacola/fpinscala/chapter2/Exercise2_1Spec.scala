package adacola.fpinscala.chapter2

import adacola.fpinscala.chapter2.Exercise2_1.{Fib, Mat}
import org.scalatest.{Matchers, FreeSpec}

class Exercise2_1Spec extends FreeSpec with Matchers {

  "Matが妥当なこと(テスト適当)" - {
    "*" in {
      val x = Mat(IndexedSeq(IndexedSeq(2, 3), IndexedSeq(1, 4), IndexedSeq(2, 1)))
      val y = Mat(IndexedSeq(IndexedSeq(3, 1, 2), IndexedSeq(2, 4, 2)))
      x * y shouldBe Mat(IndexedSeq(IndexedSeq(12, 14, 10), IndexedSeq(11, 17, 10), IndexedSeq(8, 6, 6)))
    }

    "E" in {
      Mat.E(2) shouldBe Mat(IndexedSeq(IndexedSeq(1, 0), IndexedSeq(0, 1)))
      Mat.E(3) shouldBe Mat(IndexedSeq(IndexedSeq(1, 0, 0), IndexedSeq(0, 1, 0), IndexedSeq(0, 0, 1)))
    }
  }

  "fibMatが素朴なフィボナッチ数列の結果と一致すること" in {
    def fibNaive(n: Int): Int = n match {
      case 0 => 0
      case 1 => 1
      case _ => fibNaive(n - 1) + fibNaive(n - 2)
    }

    for (i <- 0 to 20) {
      Fib.fibMat(i) shouldBe fibNaive(i)
    }
  }

}
