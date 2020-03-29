package chapter4

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class OptionSpec extends AnyFreeSpec with Matchers {

  "map2" - {
    "should return map2" in {
      def add(a: Int, b: Int): Int = a + b
      val optAge = Some(2)
      val optTickets = Some(3)
      Option.map2(optAge, optTickets)(add)
    }
  }
}
