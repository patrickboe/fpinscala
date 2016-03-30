sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](t: Tree[A]) : Int = t match {
    case Leaf(x) => 1
    case Branch(l,r) => 1 + size(l) + size(r)
  }

  def maximum(t: Tree[Int]) : Int = t match {
    case Leaf(x) => x
    case Branch(l,r) => maximum(l) max maximum(r)
  }

  def depth[A](t: Tree[A]) : Int = t match {
    case Leaf(x) => 0
    case Branch(l,r) => 1 + (depth(l) max depth(r))
  }

  def map[A,B](t: Tree[A])(f: A => B) : Tree[B] = t match {
    case Leaf(x) => Leaf(f(x))
    case Branch(l,r) => Branch(map(l)(f),map(r)(f))
  }

  def fold[A,B](t: Tree[A])(fleaf: A => B)(fbranch: (B,B) => B) : B = t match {
    case Leaf(x) => fleaf(x)
    case Branch(l,r) => fbranch(fold(l)(fleaf)(fbranch),fold(r)(fleaf)(fbranch))
  }

  def sizef[A](t: Tree[A]) : Int = fold(t)(x=>1)((x,y)=>1+x+y)

  def maximumf(t: Tree[Int]) : Int = fold(t)(x=>x)((x,y)=> x max y)

  def depthf[A](t: Tree[A]) : Int = fold(t)(x=>0)((x,y)=> 1 + (x max y))

  def mapf[A,B](t: Tree[A])(f: A => B) : Tree[B] =
    fold(t)(x=>Leaf(f(x)):Tree[B])((x,y)=>Branch(x,y))
}

// vim: set ts=2 sw=2 et:
