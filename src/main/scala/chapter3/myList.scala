package chapter3

sealed trait MyList[+A]
case object Nil extends MyList[Nothing]
case class Cons[+A](head: A, tail: MyList[A]) extends MyList[A]

object MyList {

  def apply[A](as: A*): MyList[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def sum(ints: MyList[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, tail) => x + sum(tail)
  }

  def product(ints: MyList[Int]): Int = ints match {
    case Nil => 1
    case Cons(0, _) => 0
    case Cons(x, tail) => x * product(tail)
  }

  def tail[A](list: MyList[A]): MyList[A] = list match {
    case Nil => Nil
    case Cons(_, tail) => tail
  }

  def setHead[A](list: MyList[A], newHead: A): MyList[A] = list match {
    case Nil => Nil
    case Cons(_, tail) => Cons(newHead, tail)
  }

  def drop[A](list: MyList[A], n: Int): MyList[A] = list match {
    case Nil => Nil
    case Cons(x, tail) if n == 0 => Cons(x, tail)
    case Cons(x, tail) => drop(tail, n - 1)
  }

  def dropWhile[A](list: MyList[A], predicate: A => Boolean): MyList[A] = list match {
    case Nil => Nil
    case Cons(x, tail) if predicate(x) => dropWhile(tail, predicate)
    case x => x
  }

  def append[A](a1: MyList[A], a2: MyList[A]): MyList[A] = a1 match {
    case Nil => a2
    case Cons(h, t) => Cons(h, append(t,  a2))
  }

  def init[A](list: MyList[A]): MyList[A] = {
    def loop(list: MyList[A], acc: MyList[A]): MyList[A] = list match {
      case Nil => Nil
      case Cons(_, Nil) => acc
      case Cons(x, tail) => loop(tail, append(acc, Cons(x, Nil)))
    }
    loop(list, Nil)
  }





}

object main extends App {
  import MyList._

  println(init(MyList(1,2,3,4)))
}


