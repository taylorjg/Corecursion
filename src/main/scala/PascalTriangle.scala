object PascalTriangle {
  def generate: Stream[Seq[Int]] = {
    def helper(prev: Seq[Int]): Stream[Seq[Int]] = {
      val next = prev match {
        case Seq() => Seq(1)
        case Seq(1) => Seq(1, 1)
        case _ =>
          val middlePairs = prev sliding 2
          val middleValues = middlePairs map (_.sum)
          Seq(1) ++ middleValues ++ Seq(1)
      }
      next #:: helper(next)
    }
    helper(Seq())
  }
}
