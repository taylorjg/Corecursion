case class Tree[A](value: A, left: Option[Tree[A]], right: Option[Tree[A]])

object Tree {

  def depthFirst[A](tree: Tree[A])(f: A => Unit): Unit = {
    tree.left foreach (depthFirst(_)(f))
    tree.right foreach (depthFirst(_)(f))
    f(tree.value)
  }

  def depthFirstMap[A, B](tree: Tree[A])(f: A => B): Tree[B] =
    tree match {
      case Tree(v, Some(l), Some(r)) =>
        Tree[B](f(v), Some(depthFirstMap(l)(f)), Some(depthFirstMap(r)(f)))
      case Tree(v, Some(l), None) =>
        Tree[B](f(v), Some(depthFirstMap(l)(f)), None)
      case Tree(v, None, Some(r)) =>
        Tree[B](f(v), None, Some(depthFirstMap(r)(f)))
      case Tree(v, None, None) => Tree(f(v), None, None)
    }

  def breadthFirst[A](tree: Tree[A]): Stream[A] = {
    def helper(trees: Stream[Tree[A]]): Stream[A] = {
      trees match {
        case Stream.Empty => Stream.empty
        case _ =>
          val seed = (Stream.empty[A], Stream.empty[Tree[A]])
          val (values, newTrees) = trees.foldLeft(seed)((acc, t) => {
            val (vs, nts) = acc
            (vs #::: Stream(t.value), nts #::: t.left.toStream #::: t.right.toStream)
          })
          values #::: helper(newTrees)
      }
    }
    helper(Stream(tree))
  }

  def format[A](node: Tree[A]): String =
    formatLevel(node, 0)

  private def formatLevel[A](node: Tree[A], level: Int): String = {
    val indent = List.fill(level * 2)(' ').mkString("")
    val line1 = node.value.toString
    val line2 = node.left match {
      case Some(leftNode) => s"\n${indent}Left:  ${formatLevel(leftNode, level + 1)}"
      case None => ""
    }
    val line3 = node.right match {
      case Some(rightNode) => s"\n${indent}Right: ${formatLevel(rightNode, level + 1)}"
      case None => ""
    }
    s"$line1$line2$line3"
  }
}
