import org.scalatest.FunSpec

class PascalTriangleTests extends FunSpec {

  describe("Pascal Triangle") {

    describe("generate") {

      it("should have correct level 0") {
        assert(PascalTriangle.generate.head == Seq(1))
      }

      it("should have correct level 1") {
        assert(PascalTriangle.generate(1) == Seq(1, 1))
      }

      it("should have correct level 2") {
        assert(PascalTriangle.generate(2) == Seq(1, 2, 1))
      }

      it("should have correct level 3") {
        assert(PascalTriangle.generate(3) == Seq(1, 3, 3, 1))
      }

      it("should have correct level 4") {
        assert(PascalTriangle.generate(4) == Seq(1, 4, 6, 4, 1))
      }

      it("should have correct level 5") {
        assert(PascalTriangle.generate(5) == Seq(1, 5, 10, 10, 5, 1))
      }
    }

    describe("generate2") {

      it("should have correct level 0") {
        assert(PascalTriangle.generate2.head == Seq(1))
      }

      it("should have correct level 1") {
        assert(PascalTriangle.generate2(1) == Seq(1, 1))
      }

      it("should have correct level 2") {
        assert(PascalTriangle.generate2(2) == Seq(1, 2, 1))
      }

      it("should have correct level 3") {
        assert(PascalTriangle.generate2(3) == Seq(1, 3, 3, 1))
      }

      it("should have correct level 4") {
        assert(PascalTriangle.generate2(4) == Seq(1, 4, 6, 4, 1))
      }

      it("should have correct level 5") {
        assert(PascalTriangle.generate2(5) == Seq(1, 5, 10, 10, 5, 1))
      }
    }
  }
}
