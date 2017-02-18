import org.scalatest.FunSpec

class TreeTests extends FunSpec {

  private final val SampleTree = Tree(
    30,
    Some(Tree(12, Some(Tree(7, None, None)), Some(Tree(11, None, None)))),
    Some(Tree(41, None, None)))

  describe("Tree") {

    it("depthFirstMap returns correct tree") {
      val actual = Tree.depthFirstMap(SampleTree)(_ + 1)
      val expected = Tree(
        31,
        Some(Tree(13, Some(Tree(8, None, None)), Some(Tree(12, None, None)))),
        Some(Tree(42, None, None)))
      assert(actual == expected)
    }

    it("breadthFirst returns items in breadth first order") {
      val actual = Tree.breadthFirst(SampleTree)
      assert(actual == Seq(30, 12, 41, 7, 11))
    }
  }
}
