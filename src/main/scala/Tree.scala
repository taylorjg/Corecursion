case class Tree[A](value: A, left: Option[Tree[A]], right: Option[Tree[A]])

object Tree {

  def format[A](node: Tree[A]): String = {
    formatLevel(node, 0)
  }

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
