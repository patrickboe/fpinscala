object ListPlus {
  //Ex 24
  def hasSubsequence[A](sup: List[A], sub: List[A]):Boolean = sup match {
    case Nil => false
    case x :: xs => sub match {
      case Nil => true
      case y :: ys => (x == y && hasSubsequence(xs,ys)) || hasSubsequence(xs,sub)
    }
  }
}


// vim: set ts=2 sw=2 et:
