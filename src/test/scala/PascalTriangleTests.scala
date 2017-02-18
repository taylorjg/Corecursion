import org.scalatest.FunSpec

class PascalTriangleTests extends FunSpec {

  describe("Pascal Triangle") {

    it("should have correct level 0") {
      val actual = PascalTriangle.generate.head
      assert(actual == Seq(1))
    }

    it("should have correct level 1") {
      val actual = PascalTriangle.generate(1)
      assert(actual == Seq(1, 1))
    }

    it("should have correct level 2") {
      val actual = PascalTriangle.generate(2)
      assert(actual == Seq(1, 2, 1))
    }

    it("should have correct level 3") {
      val actual = PascalTriangle.generate(3)
      assert(actual == Seq(1, 3, 3, 1))
    }

    it("should have correct level 4") {
      val actual = PascalTriangle.generate(4)
      assert(actual == Seq(1, 4, 6, 4, 1))
    }

    it("should have correct level 5") {
      val actual = PascalTriangle.generate(5)
      assert(actual == Seq(1, 5, 10, 10, 5, 1))
    }
  }
}
