package org.tabiul.fpinscala.chapter2

import scala.annotation.tailrec

object Problems {

    def fib(n: Int): Int = {
        @tailrec
        def fib(curr: Int, prev1: Int, prev2: Int): Int = {
            if (curr == n) prev1 + prev2
            else if (curr == 0) fib(curr + 1, 0, 0)
            else if (curr == 1) fib(curr + 1, 0, 1)
            else fib(curr + 1, prev2, prev1 + prev2)
        }

        fib(0, 0, 0)
    }

    def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
        if (as.length == 0) true
        else {
            val len = as.length - 1
            def isSorted(index: Int): Boolean = {
                if (index == len) true
                else {
                    if (ordered(as(index), as(index + 1))) isSorted(index + 1)
                    else false
                }
            }

            isSorted(0)
        }
    }

    def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
        a: A => b: B => f(a, b)
    }

    def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
        (a: A, b: B) => f(a)(b)
    }

    def compose[A, B, C](f: B => C, g: A => B): A => C = {
        a: A => f(g(a))
    }

}