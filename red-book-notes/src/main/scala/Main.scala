import foo.HowToImplementCompose.{composedFunc2}

import scala.Console.println
import scala.annotation.tailrec

object foo {

  def main(args: Array[String]): Unit = {
    //        println(isSorted(Array(1, 2, 3, 4, 5), (a, b) => a <= b))
    //        println(isSorted(Array(1, 2, 4, 8, 16), (a, b) => a * 2 == b))
    //        println(factorialTR(3))
    //        println(composedFunc2(5))
    //    println(DataStructures.drop(List(1, 2, 3, 4, 5), 2))
    println(foo.DataStructures.drop(List(1, 2, 3, 4, 5), 2))
  }

  // EXERCISE 2.1 reccursive Fibonacci
  def getNFib(n: Int): Int = {
    def go(a: Int, b: Int, n: Int): Int = {
      if (n <= 0) {
        a + b
      } else {
        go(b, a + b, n - 1)
      }

    }

    go(0, 1, n - 3)
  }

  def fib(n: Int): Int = {
    if (n <= 2) 1
    else {
      fib(n - 1) + fib(n - 2)
    }
  }
  //  How fib(4) works
  //  fib(4) calls fib(3) and fib(2).
  //  fib(3) calls fib(2) and fib(1).
  //  fib(2) returns 1.
  //  fib(1) returns 1.
  //  fib(3) adds the results of fib(2) and fib(1) (1 + 1) and returns 2.
  //  fib(4) adds the results of fib(3) and fib(2) (2 + 1) and returns 3, which is the correct answer for fib(4).


  // EXERCISE 2.2 is sorted
  //  Implement isSorted, which checks whether an Array[A]
  //  is sorted according to a given comparison function:

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {

    def loop(n: Int): Boolean = {
      if (n >= as.length - 1) {
        true
      } else if (ordered(as(n), as(n + 1))) {
        loop(n + 1)
      } else {
        false
      }

    }

    loop(0)
  }


  // Recursive Factorial
  def factorial(n: Int): Int =
    if (n == 0) 1
    else n * factorial(n - 1)

  // Tail Recursive factorial
  def factorialTR(n: Int): Int = {

    @tailrec def loop(acc: Int, timesBy: Int): Int = {
      if (timesBy <= 2) {
        acc
      } else
        val foo = timesBy - 1
        loop(acc * foo, foo)
    }

    loop(n, n)
  }

  def factorialTRChatGPT(n: Int): Int = {
    @tailrec
    def factorialHelper(n: Int, accumulator: Int): Int = {
      if (n <= 1) accumulator
      else factorialHelper(n - 1, n * accumulator)
    }

    factorialHelper(n - 1, 1)
  }


  // currying

  // EXERCISE 2.3
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    a => b => f(a, b)
  }

  // or
  def curry2[A, B, C](f: (A, B) => C): A => (B => C) = {
    (a: A) => (b: B) => f(a, b)
  }


  object HowToImplementCurry {

    // Define a function that takes two arguments and returns their sum
    def sum(a: Int, b: Int): Int = a + b

    // Use the curry function to create a curried version of the sum function
    val curriedSum: Int => (Int => Int) = curry(sum)

    // Call the curriedSum function with the first argument
    val add5: Int => Int = curriedSum(5)

    // Call the add5 function with the second argument
    val result: Int = add5(3)

    println(result) // Output: 8

  }


  def unCurry[A, B, C](f: A => B => C): (A, B) => C = {

    (a, b) => f(a)(b)
  }


  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    (a: A) => f(g(a))
  }

  def isEven(a: Int): Boolean = a % 2 == 0

  def logResult(b: Boolean): String = s"The result was $b"

  object HowToImplementCompose {

    def isEven(a: Int): Boolean = a % 2 == 0

    def logResult(b: Boolean): String = s"The result was $b"

    val composedFunc2: Int => String = compose(logResult, isEven)

    //    println(composedFunc2(10)) // Output: "The result was true"

    // Function 1: Double a number
    def double(x: Int): Int = x * 2

    // Function 2: Add 1 to a number
    def addOne(x: Int): Int = x + 1

    // Using compose to combine the functions
    val composedFunction: Int => Int = compose(double, addOne)

    // Applying the composed function to an input value
    //    println(composedFunction(3)) // Output: 8

  }


  // EXERCISE 3.2
  object DataStructures {
    def tail[A](x: List[A]): List[A] = {
      x match {
        case nil => x
        case y => y.drop(1)
        //      case Cons(_,t) => t // APPARENTLY THE CORRECT ANSWER
      }
    }

    // Multiple ways to do setHead better than the redbook
    object ManySetHeads {
      def setHead[A](list: List[A], newHead: A): List[A] = newHead :: list.tail

      def setHeadPM[A](list: List[A], newHead: A): List[A] = list match {
        case Nil => List(newHead)
        case _ :: tail => newHead :: tail
      }

      def setHeadUpdated[A](list: List[A], newHead: A): List[A] = list.updated(0, newHead)

    }

    def drop[A](x: List[A], dropN: Int): List[A] = {
      def dropRec[A](x: List[A], acc: Int): List[A] = {
        if (acc <= 0) {
          x
        } else {
          x match {
            case Nil => x
            case _ :: t => dropRec(t, acc - 1)
          }
        }
      }

      dropRec(x, dropN)

    }

    def myTail[A](x: List[A]): List[A] = {
      x match {
        case Nil => x
        case _ :: t => t
      }
    }

    def mySetHead[A](x: List[A], head: A): List[A] = {
      x match {
        case Nil => List(head)
        case _ :: t => head :: t

      }
    }


  }

}


