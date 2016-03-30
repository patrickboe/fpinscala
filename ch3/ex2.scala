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

  //Ex 7
  //no it cannot halt the recursion. it is subject to the evaluation rule of foldRight, which is to evaluate all the arguments and then apply the function to them. there is no allowance in foldRight for a short-circuiting value. a different implementation of foldRight which short-circuited on a third argument could do this.

  //Ex 8
  //this is the identity function. foldRight is a kind of superset of the List constructor.

  //Ex 9
  def foldRight[A,B](as: List[A], z: B)(f: (A,B)=>B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def length[A](as: List[A]): Int = foldRight(as,0)((_,n) => n + 1)

  //Ex 10
  @annotation.tailrec
  def foldLeft[A,B](as: List[A], z: B)(f: (B,A)=>B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z,x))(f)
    }

  //Ex 11
  def sum(xs: List[Int]): Int = List.foldLeft(xs,0)(_+_)

  def product(xs: List[Int]): Int = List.foldLeft(xs,1)(_*_)

  def len[A](as: List[A]): Int = foldLeft(as,0)((n,_) => n + 1)

  //Ex 12
  def reverse[A](as: List[A]): List[A] = foldLeft(as,Nil:List[A])((xs,x)=>Cons(x,xs))

  //Ex 13
  def flip[A,B,C](f: (A,B)=>C): (B,A)=>C = (x:B,y:A) => f(y,x)

  def foldLeftR[A,B](as: List[A], z: B)(f: (B,A)=>B): B =
    foldRight(reverse(as), z)(flip(f))

  def foldRightL[A,B](as: List[A], z: B)(f: (A,B)=>B): B =
    foldLeft(reverse(as), z)(flip(f))

  //Ex 14
  def append[A](xs: List[A], ys: List[A]): List[A] =
    foldRight(xs,ys)(Cons(_,_))

  //Ex 15
  def flatten[A](xs: List[List[A]]) : List[A] =
    foldRight(xs,Nil:List[A])(append)
}
