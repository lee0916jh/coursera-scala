package recfun

import scala.annotation.tailrec

object RecFun extends RecFunInterface :

  def main(args: Array[String]): Unit =
    println("Pascal's Triangle")
    for row <- 0 to 10 do
      for col <- 0 to row do
        print(s"${pascal(col, row)} ")
      println()

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int =
    if c == 0 || c == r then 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean =
    @tailrec
    def aux(chars: List[Char], stack: List[Char]): Boolean =
      if chars.isEmpty && stack.isEmpty then return true
      if chars.isEmpty && stack.nonEmpty then return false

      if chars.head == '(' then aux(chars.tail, chars.head :: stack)
      else if chars.head == ')' && stack.isEmpty then false
      else if chars.head == ')' && stack.head == '(' then aux(chars.tail, stack.tail)
      else aux(chars.tail, stack)

    aux(chars, Nil)


  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int =
    if money < 0 || money > 0 && coins.isEmpty then 0
    else if money == 0 then 1
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)