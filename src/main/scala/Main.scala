object Main {
  def main(args: Array[String]): Unit = {
    val tree = Node(
      30,
      Some(Node(12, Some(Node(7, None, None)), Some(Node(11, None, None)))),
      Some(Node(41, None, None)))
    println(Node.format(tree))
    depthFirst[Int](println)(tree)
    breadthFirst(tree).toIterator foreach println
  }

  private def depthFirst[A](f: A => Unit)(node: Node[A]): Unit = {
    node.left foreach depthFirst(f)
    node.right foreach depthFirst(f)
    f(node.value)
  }

  private def breadthFirst[A](node: Node[A]): Stream[A] = {
    def helper(nodes: List[Node[A]]): Stream[A] = {
      nodes match {
        case Nil => Stream.empty
        case _ =>
          val seed = (List.empty[A], List.empty[Node[A]])
          val (v3, v4) = nodes.foldLeft(seed)((acc, n) => {
            val (v1, v2) = acc
            (n.value :: v1, n.left.toList ++ n.right.toList ++ v2)
          })
          v3.reverse.toStream #::: helper(v4)
      }
    }
    helper(List(node))
  }
}
