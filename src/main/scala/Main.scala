object Main {

  private final val SampleTree = Tree(
    30,
    Some(Tree(12, Some(Tree(7, None, None)), Some(Tree(11, None, None)))),
    Some(Tree(41, None, None)))

  def main(args: Array[String]): Unit = {
    println(Tree.format(SampleTree))
    println
    println(Tree.format(Tree.depthFirstMap(SampleTree)(_ + 1)))
    println
    Tree.depthFirst(SampleTree)(println)
    println
    Tree.breadthFirst(SampleTree) foreach println
    println
  }
}
