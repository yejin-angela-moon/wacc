package wacc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import parsley.Failure
import parsley.Success

class ParserSpec extends AnyFlatSpec with Matchers {

  "The parser" should "correctly parse simple addition" {
    val result = parser.parse("1 + 1")
    result shouldBe a[Success[_]]
    result.get shouldEqual 2
  }

  it should "correctly parse simple subtraction" in {
    val result = parser.parse("2 - 1")
    result shouldBe a[Success[_]]
    result.get shouldEqual 1
  }

  it should "return Failure for invalid input" in {
    val result = parser.parse("invalid")
    result shouldBe a[Failure]
  }

  
}