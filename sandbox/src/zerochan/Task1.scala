package zerochan

object Task1 {
	
  def main(args: Array[String]): Unit = {
    
    val list = List(1, 5, 10, 11)
    
    val sorted = list.sortWith((a, b) => {
      
      def sum0(num: Int, sum: Int): Int = num match {
      	case 0 => sum
      	case _ => sum0(num / 10, sum + num % 10)
      }
      sum0(a, 0) < sum0(b, 0)
    })
    
    println(sorted)
  }
  
}