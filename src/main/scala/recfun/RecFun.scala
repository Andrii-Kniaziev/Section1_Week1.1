package recfun

import java.util

object RecFun extends RecFunInterface:

  def main(args: Array[String]): Unit =
    println("Pascal's Triangle")
    for row <- 0 to 10 do
      for col <- 0 to row do
        print(s"${pascal(col, row)} ")
      println()

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    def getRow(r: Int): List[Int] = {
      val currow = List(1)

      if (r == 0) return currow

      val prev = getRow(r - 1)

      def acc(to: List[Int], from: List[Int]): List[Int] = from match {
        case Nil => to
        case x :: xs => xs match {
          case Nil => to ++ List(x)
          case y :: ys => acc(to ++ List(x + y), xs)
        }
      }

      acc(currow, prev)
    }

    getRow(r)(c)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balanced(chrs: List[Char], opened: Int): Boolean = {
      if (chrs.isEmpty) opened == 0
      else
        if (chrs.head == '(') balanced(chrs.tail,opened+1)
        else
          if (chrs.head == ')') opened>0 && balanced(chrs.tail,opened-1)
          else balanced(chrs.tail,opened)
    }
    balanced(chars,0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if(money == 0)
      1
    else if(money > 0 && !coins.isEmpty)
      countChange(money - coins.head, coins) + countChange(money, coins.tail)
    else
      0
  }

