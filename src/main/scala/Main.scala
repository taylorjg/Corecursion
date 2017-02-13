object Main {

  private final val SampleTree = Tree(
    30,
    Some(Tree(12, Some(Tree(7, None, None)), Some(Tree(11, None, None)))),
    Some(Tree(41, None, None)))

  def main(args: Array[String]): Unit = {
    println(Tree.format(SampleTree))
    println
    println(Tree.format(depthFirstMap(SampleTree)(_ + 1)))
    println
    depthFirst(SampleTree)(println)
    println
    breadthFirst(SampleTree) foreach println
    println
  }

  private def depthFirst[A](tree: Tree[A])(f: A => Unit): Unit = {
    tree.left foreach (depthFirst(_)(f))
    tree.right foreach (depthFirst(_)(f))
    f(tree.value)
  }

  private def depthFirstMap[A, B](tree: Tree[A])(f: A => B): Tree[B] =
    tree match {
      case Tree(v, Some(l), Some(r)) =>
        Tree[B](f(v), Some(depthFirstMap(l)(f)), Some(depthFirstMap(r)(f)))
      case Tree(v, Some(l), None) =>
        Tree[B](f(v), Some(depthFirstMap(l)(f)), None)
      case Tree(v, None, Some(r)) =>
        Tree[B](f(v), None, Some(depthFirstMap(r)(f)))
      case Tree(v, None, None) => Tree(f(v), None, None)
    }

  private def breadthFirst[A](tree: Tree[A]): Stream[A] = {
    def helper(trees: List[Tree[A]]): Stream[A] = {
      trees match {
        case Nil => Stream.empty
        case _ =>
          val seed = (List.empty[A], List.empty[Tree[A]])
          val (values, newTrees) = trees.foldLeft(seed)((acc, t) => {
            val (vs, nts) = acc
            (t.value :: vs, t.left.toList ++ t.right.toList ++ nts)
          })
          values.reverse.toStream #::: helper(newTrees)
      }
    }
    helper(List(tree))
  }
}
