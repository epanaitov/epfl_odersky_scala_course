object test_ws {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
		def rec(x: Int): Int = if (x == 1) 1 else rec(x - 1)
                                                  //> rec: (x: Int)Int
		
		rec(24)                           //> res0: Int = 1
}