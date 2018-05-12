package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
    
    println("Parentheses Balancing")
    val pb1 = "(if (zero? x) max (/ 1 x))"
    val pb2 = "I told him (that it’s not (yet) done). (But he wasn’t listening)"
    val pb3 = ":-)"
    val pb4 = "())("
    
    println(pb1 + ": " + balance(pb1.toList))
    println(pb2 + ": " + balance(pb2.toList))
    println(pb3 + ": " + balance(pb3.toList))
    println(pb4 + ": " + balance(pb4.toList))
    
    println("Counting Change")
    println("changing 4 bucks with [1,2]: " + countChange(4, List(1, 2)))
    println("changing 100 bucks with [1,5,10,25,50]: " + countChange(100, List(1,5,10,25,50)))
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
	  
	  if ((c == 0) || (c == r)) 1
	  else {
	 	  if (r < 0) throw new IllegalArgumentException("row must be >= 0") 
          else if (r == 0) 1 
          else pascal(c, r-1) + pascal(c-1, r-1)
	  }
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
	  
	  def balanceIter(chars: List[Char], opens: Int, closes: Int): Boolean = {
	 	  
	 	  if (chars.isEmpty) (opens == closes) else {
	 	  
    	 	  val symbol = chars.head
    	 	  
    	 	  symbol match {
    	 	 	  case '(' => balanceIter(chars.tail, opens+1, closes)
    	 	 	  case ')' => {
    	 	 	      if (closes == opens) false
    	 	 	      else balanceIter(chars.tail, opens, closes+1)
    	 	 	  }
    	 	 	  case _ => balanceIter(chars.tail, opens, closes) 
    	 	  }
	 	  }
	  }
	  
	  balanceIter(chars, 0, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
	  if (coins.isEmpty) 0 else {
	 	  money match {
	 	 	  case 0 => 1
	 	 	  case x if x < 0 => 0
	 	 	  case _ => countChange(money - coins.head, coins) + countChange(money, coins.tail) 
	 	  }
	  }
  }
}
