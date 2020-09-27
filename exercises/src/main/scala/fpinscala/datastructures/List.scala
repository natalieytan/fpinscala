package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  // Exercise 3.2
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, tail) => tail
  }

  // Exercise 3.3
  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Nil
    case Cons(_, tail) => Cons(h, tail)
  }

  // Exercise 3.4
  @scala.annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0 || l == Nil) {
      l
    } else {
      drop(tail(l), n - 1)
    }
  }

  // Exercise 3.5
  @scala.annotation.tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Nil => Nil
      case Cons(head, tail) => if (f(head)) {
        dropWhile(tail, f)
      } else {
        Cons(head, tail)
      }
    }

  }

  // Exercise 3.6
  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil // do not add head
    case Cons(head, tail) => Cons(head, init(tail))
  }

  // Exercise 3.9
  def length[A](l: List[A]): Int = foldRight(l, 0)((_, acc) => acc + 1)

  // Exercise 3.10
  @scala.annotation.tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(head, tail) => foldLeft(tail, f(z, head))(f)
  }

  // Exercise 3.11
  def sum3(l: List[Int]): Int = foldLeft(l, 0)(_ + _)

  def product3(l: List[Double]): Double = foldRight(l, 1.0)(_ * _)

  // Exercise 3.12
  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A])(
    (accumulator, head) => Cons(head, accumulator)
  )

  // Exercise 3.13
  def foldRightViaFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(reverse(as), z)((accumulator, head) => f(head, accumulator))
  }


  // Exercise 3.14
  def appendViaFoldRight[A](a1: List[A], a2: List[A]): List[A] = {
    foldRight(a1, a2)(
      (head, tail) => {
        Cons(head, tail)
      }
    )
  }

  // Exercise 3.15
  def concat[A](l: List[List[A]]): List[A] = {
    foldRight(l, Nil: List[A])((head, accumulator) => {
      foldRight(head, accumulator)((insideHead, insideAccumulator) => {
        Cons(insideHead, insideAccumulator)
      })
    })
  }

  def concatViaAppend[A](l: List[List[A]]): List[A] = foldRight(l, Nil: List[A])(append)

  // Exercise 3.16
  def addOneEachToList(l: List[Int]): List[Int] = {
    l match {
      case Nil => Nil
      case Cons(head, tail) => Cons(head + 1, addOneEachToList(tail))
    }
  }

  def addOneEachToListViaFoldRight(l: List[Int]): List[Int] = {
    foldRight(l, Nil: List[Int])((head, tail) => Cons(head + 1, tail))
  }


  // Exercise 3.17
  def eachDoubleToString(l: List[Double]): List[String] = {
    l match {
      case Nil => Nil
      case Cons(head, tail) => Cons(head.toString, eachDoubleToString(tail))
    }
  }

  def eachDoubleToStringViaFoldRight(l: List[Double]): List[String] = foldRight(l, Nil: List[String])(
    (head, tail) => Cons(head.toString, tail)
  )

  // Exercise 3.18
  def map[A, B](l: List[A])(f: A => B): List[B] = foldRight(l, Nil: List[B])((head, tail) => Cons(f(head), tail))

  // Exercise 3.19
  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    foldRight(as, Nil: List[A])((head, accumulator) => {
      if(f(head)) {
        Cons(head, accumulator)
      } else {
        accumulator
      }
    })
  }

  // Exercise 3.20
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
    val nestedList = map(as)(f)
    foldRight(nestedList, Nil: List[B])((outsideHead, outsideAccumulator) => {
      foldRight(outsideHead, outsideAccumulator)((insideHead, insideAcc) => {
        Cons(insideHead, insideAcc)
      })
    })
  }

  def flatMapViaConcat[A, B](as: List[A])(f: A => List[B]): List[B] =
    concat(map(as)(f))

  // Exercise 3.21
  def filterViaFlatMap[A](as: List[A])(f: A => Boolean): List[A] = {
    flatMap(as)(head =>
      if(f(head)) {
      Cons(head, Nil)
    } else {
      Nil
    })
  }

  // Exercise 3.22
  def addTwoListIntoOne(a: List[Int], b: List[Int]): List[Int] = {
    (a,b) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(head1, tail1), Cons(head2, tail2)) =>
        Cons(head1 + head2, addTwoListIntoOne(tail1, tail2))
    }
  }


  // Exercise 3.23
  def zipWith[A, B, C](a: List[A], b: List[B])(f: (A, B) => C): List[C] = {
    (a, b) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(head1, tail1), Cons(head2, tail2)) =>
        Cons(f(head1, head2), zipWith(tail1, tail2))
    }
  }

  // Exercise 3.24
  @scala.annotation.tailrec
  def startsWithSequence[A](a: List[A], b: List[A]): Boolean = {
    (a, b) match {
      case (_, Nil) => true
      case (Cons(aHead, aTail), Cons(bHead, bTail)) =>
        if(aHead == bHead) {
          startsWithSequence(aTail, bTail)
        } else {
          false
        }
    }
  }

  @scala.annotation.tailrec
  def hasSubsequence[A](a: List[A], b: List[A]): Boolean = {
    a match {
      case Nil => b == Nil
      case Cons(head, tail) =>
        if(startsWithSequence(a, b)) {
          true
        } else {
          hasSubsequence(tail, b)
        }
    }
  }
}
