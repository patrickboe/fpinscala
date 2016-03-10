sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  //Ex 2
  def tail[A](l: List[A]) = l match {
    case Nil => Nil
    case Cons(x, xs) => xs
  }

  //Ex 3
  def setHead[A](l: List[A], y: A) = l match {
    case Nil => Nil
    case Cons(x, xs) => Cons(y, xs)
  }

  //Ex 4
  @annotation.tailrec
  def drop[A](l: List[A],n: Int): List[A] =
    if(n==0) l
    else l match {
      case Nil => Nil
      case Cons(x, xs) => drop(xs, n - 1)
    }

  //Ex 5
  @annotation.tailrec
  def dropWhile[A](l: List[A],f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) => if(!f(x)) l else dropWhile(xs, f)
  }

  //Ex 6
  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x, Cons(y, Nil)) => List(x)
    case Cons(x, xs) => Cons(x, init(xs))
  }
}
