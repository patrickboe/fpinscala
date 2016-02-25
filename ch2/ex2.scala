object MySort {
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean =
      n==as.length-1 || ordered(as(n),as(n+1)) && loop(n + 1)

    loop(0)
  }
}

// vim: set ts=2 sw=2 et:
