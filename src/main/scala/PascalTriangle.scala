object PascalTriangle {

  def generate: Stream[Seq[Int]] = {
    def worker(prev: Seq[Int]): Stream[Seq[Int]] = {
      val next = prev match {
        case Seq() => Seq(1)
        case Seq(1) => Seq(1, 1)
        case _ =>
          val middlePairs = prev sliding 2
          val middleValues = middlePairs map (_.sum)
          Seq(1) ++ middleValues ++ Seq(1)
      }
      next #:: worker(next)
    }
    worker(Seq())
  }

  def generate2: Stream[Seq[Int]] = {
    def worker(n: Int): Stream[Seq[Int]] =
      (1 to n).foldLeft(List(1))((xs, k) => {
        xs.head * (n + 1 - k) / k :: xs
      }) #:: worker(n + 1)
    worker(0)
  }
}
