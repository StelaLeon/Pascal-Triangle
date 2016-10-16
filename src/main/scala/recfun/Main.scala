package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }

    println("trying the second assignment: "+ balance("(:)".toList))

    val listOfCoins = List(1,2,3,7)
    val possibilities = countChange(90,listOfCoins)
    println(s"we got the possibilities: ${possibilities}")
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = {
    if(c==0 || c==r)
      return 1
    else{
      return pascal(c-1,r-1) + pascal(c,r-1)
    }
  }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def balanceChars(subChar: List[Char], noOfOpenedP: Int, noOfClosedP: Int): Boolean = {
        if (noOfOpenedP < noOfClosedP)
          false
        else if (subChar.isEmpty)
          true
        else if (subChar.head.equals('('))
          balanceChars(subChar.tail, noOfOpenedP + 1, noOfClosedP)
        else if (subChar.head.equals(')'))
          balanceChars(subChar.tail, noOfOpenedP, noOfClosedP + 1)
        else
          balanceChars(subChar.tail, noOfOpenedP, noOfClosedP)
      }
    balanceChars(chars,0,0)
  }
  
  /**
   * Exercise 3
   */

  def countChange(money: Int, coins: List[Int]): Int = {

      if(money>0 && coins.isEmpty)
        return 0
      if(money <0)
        return 0

      if(money == 0)
        return 1

    countChange(money-coins.head,coins) + countChange(money,coins.tail)

  }
  }
