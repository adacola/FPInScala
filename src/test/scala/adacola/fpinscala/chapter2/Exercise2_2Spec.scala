package adacola.fpinscala.chapter2

import org.scalatest.{Matchers, FreeSpec}

class Exercise2_2Spec extends FreeSpec with Matchers {

  "isOrderedは空の配列の場合はtrueを返すこと" in {
    Exercise2_2.isSorted[Int](Array[Int](), (_, _) => true) shouldBe true
    Exercise2_2.isSorted[Int](Array[Int](), (_, _) => false) shouldBe true
  }

  "isOrderedは要素1つの配列の場合はtrueを返すこと" in {
    Exercise2_2.isSorted[Int](Array[Int](1), (_, _) => true) shouldBe true
    Exercise2_2.isSorted[Int](Array[Int](1), (_, _) => false) shouldBe true
  }

  "isOrderedは要素2つの配列の場合はaOrderedの結果を返すこと" in {
    Exercise2_2.isSorted[Int](Array[Int](1, 2), (_, _) => true) shouldBe true
    Exercise2_2.isSorted[Int](Array[Int](1, 2), (_, _) => false) shouldBe false
  }

  "isOrderedは要素複数の配列の場合はaOrderedの結果がすべてtrueの場合のみtrueを返すこと" in {
    def ordered(x: Int, y: Int): Boolean = x <= y
    Exercise2_2.isSorted[Int](Array[Int](1, 2, 3), ordered) shouldBe true
    Exercise2_2.isSorted[Int](Array[Int](1, 2, 2), ordered) shouldBe true
    Exercise2_2.isSorted[Int](Array[Int](1, 2, 1), ordered) shouldBe false
    Exercise2_2.isSorted[Int](Array[Int](2, 1, 3), ordered) shouldBe false
  }

}
