package adacola.fpinscala.chapter2

import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Matchers, FreeSpec}

class Exercise2_3_5Spec extends FreeSpec with Matchers with GeneratorDrivenPropertyChecks {

  def f(a: Int, b: Int) = a + b

  "Exercise2-3" in {
    forAll { (a: Int, b: Int) =>
      Exercise2_3_5.curry(f)(a)(b) shouldBe f(a, b)
    }
  }

  "Exercise2-4" in {
    forAll { (a: Int, b: Int) =>
      val curried = Exercise2_3_5.curry(f)
      Exercise2_3_5.uncurry(curried)(a, b) shouldBe f(a, b)
    }
  }

  "Exercise2-5" in {
    def g(a: Int) = a * 2
    def h(b: Int) = b + 3
    def gh(a: Int) = a * 2 + 3
    forAll { (a: Int) =>
      Exercise2_3_5.compose(h, g)(a) shouldBe gh(a)
    }
  }

}
