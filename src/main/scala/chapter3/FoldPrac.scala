package chapter3

sealed trait Ginger[+A]

case object Nil extends Ginger[Nothing]

case class Cons[+A](head: A, tail: Ginger[A]) extends Ginger[A]

object Ginger {

  def tail[A](list: Ginger[A]): Ginger[A] = {
    list match {
      case Nil => sys.error("empty list")
      case Cons(_, xs) => xs
    }
  }

  def setHead[A](list: Ginger[A], rep: A): Ginger[A] = {
    list match {
      case Nil => sys.error("empty list")
      case Cons(_, tail) => Cons(rep, tail)
    }
  }

  def drop[A](list: Ginger[A], n: Int): Ginger[A] = {
    @scala.annotation.tailrec
    def dropTail(list: Ginger[A], n: Int): Ginger[A] = {
      if (n == 0) list
      else dropTail(tail(list), n - 1)
    }

    dropTail(list, n)
  }

//  def drop[A](list: Ginger[A], n: Int): Ginger[A] = {
//    if (n <= 0) list
//    else list match {
//      case Nil => sys.error("empty list")
//      case Cons(_, xs) => drop(xs, n - 1)
//    }
//  }

  def dropWhile[A](list: Ginger[A], f: A => Boolean): Ginger[A] = {
    list match {
      case Nil => sys.error("empty list")
      case Cons(x, xs) if f(x) => dropWhile(xs, f)
      case _ => list
    }
  }

  def init[A](l: Ginger[A]): Ginger[A] = {
    l match {
      case Nil => Nil
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }
  }

  def foldRight[A, B](as: Ginger[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(_, xs) => foldRight(xs, z)(f)
    }

  def sum2(ns: Ginger[Int]): Int =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: Ginger[Double]): Double =
    foldRight(ns, 1.0)(_ * _)

  def length[A](as: Ginger[A]): Int = {
    foldRight(as, 0)((_, acc) => acc + 1)
  }

  def foldLeft[A, B](as: Ginger[A], z: B)(f: (B, A) => B): B = {
    as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }
  }

  def sumFl(ns: Ginger[Int]): Int =
    foldLeft(ns, 0)((y, x) => y + x)

  def productFl(ns: Ginger[Double]): Double =
    foldLeft(ns, 1.0)((y, x) => y * x)

  def lengthFl[A](as: Ginger[A]): Int =
    foldLeft(as, 0)((acc, _) => acc + 1)

  def reverse[A](as: Ginger[A]): Ginger[A] =
    foldLeft(as, Nil: Ginger[A])((x, xs) => Cons(xs, x))

  def foldRightViaFoldLeft[A, B](as: Ginger[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), z)((b, a) => f(a, b))

  def append[A](as: Ginger[A], z: Ginger[A]): Ginger[A] =
    foldRight(as, z)(Cons(_, _))

  def concatenate[A](as: Ginger[Ginger[A]]): Ginger[A] =
    foldRight(as, Nil: Ginger[A])(append)

  def transfIntByOne(as: Ginger[Int]): Ginger[Int] =
    foldRight(as, Nil: Ginger[Int])((h, t) => Cons(h + 1, t))

  def doubleToString(as: Ginger[Double]): Ginger[String] =
    foldRight(as, Nil: Ginger[String])((x, y) => Cons(x.toString, y))

  def mapFR[A, B](as: Ginger[A])(f: A => B): Ginger[B] =
    foldRight(as, Nil: Ginger[B])((x, y) => Cons(f(x), y))

  def mapFRViaFL[A, B](as: Ginger[A])(f: A => B): Ginger[B] =
    foldRightViaFoldLeft(as, Nil: Ginger[B])((x, y) => Cons(f(x), y))

  def filter[A](as: Ginger[A])(f: A => Boolean): Ginger[A] =
    foldRight(as, Nil: Ginger[A])((x, y) => if (f(x)) Cons(x, y) else y)

  def filterFrVisaFl[A](as: Ginger[A])(f: A => Boolean): Ginger[A] =
    foldRightViaFoldLeft(as, Nil: Ginger[A])((x, y) => if (f(x)) Cons(x, y) else y)

  def flatMap[A, B](as: Ginger[A])(f: A => Ginger[B]): Ginger[B] =
    concatenate(mapFR(as)(f))

//  def FMFilter[A](as: Ginger[A])(f: A => Boolean): Ginger[A] =
//    flatMap(as)(a => if (f(a)) Ginger(a) else Nil)

  def addTwoElements(one: Ginger[Int], two: Ginger[Int]): Ginger[Int] = (one, two) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addTwoElements(t1, t2))
  }

  def zipWith[A, B, C](one: Ginger[A], two: Ginger[B])(f: (A, B) => C): Ginger[C] = (one, two) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
  }

  def startsWith[A](l: Ginger[A], prefix: Ginger[A]): Boolean = (l, prefix) match {
    case (_, Nil) => true
    case (Cons(h, t), Cons(h1, t1)) if h == h1 => startsWith(t, t1)
    case _ => false
  }

  def hasSubsequence[A](sup: Ginger[A], sub: Ginger[A]): Boolean = sup match {
    case Nil => sub == Nil
    case _ => startsWith(sup, sub)
    case Cons(_, t) => hasSubsequence(t, sub)
  }
}

