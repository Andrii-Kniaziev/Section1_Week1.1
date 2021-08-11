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
    def getRow(r: Int): util.ArrayList[Integer] = {
      val currow = new util.ArrayList[Integer]

      currow.add(1)

      if (r == 0) return currow

      val prev = getRow(r - 1)
      for (i <- 1 until prev.size) {
        val curr = prev.get(i - 1) + prev.get(i)
        currow.add(curr)
      }
      currow.add(1)

      currow
    }

    getRow(r).get(c)
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
