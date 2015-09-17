package adacola.fpinscala.chapter3

import org.scalatest.{FreeSpec, Matchers}

class Exercise3_1Spec extends FreeSpec with Matchers {

  "以下のマッチ式はどのような結果になるか" in {
    Exercise3_1.x shouldBe 3
  }

}
