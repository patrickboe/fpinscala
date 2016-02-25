object FollowTypes {
  def curry[A,B,C](f: (A,B)=>C): A => (B => C) =
    a => (b => f(a,b))

  def uncurry[A,B,C](f: A => B => C ): (A,B) => C =
    (a,b) => f(a)(b)

  def compose[A,B,C](f: B => C, g: A => B): A => C =
    a => f(g(a))
}

// vim: set ts=2 sw=2 et:
