object MyMath {
  def fib(n: Int): Int = {
    @annotation.tailrec
    def fibit(b4last: Int, last: Int, togo: Int) : Int =
      if(togo==0) last+b4last
      else fibit(last, last+b4last, togo-1)

    if(n<=1) 0
    else if(n==2) 1
    else fibit(0, 1, n-3)
  }
}

// vim: set ts=2 sw=2 et:
