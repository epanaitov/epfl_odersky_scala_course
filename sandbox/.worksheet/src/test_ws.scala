object test_ws {import scala.runtime.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(60); 
  println("Welcome to the Scala worksheet");$skip(58); 
  
		def rec(x: Int): Int = if (x == 1) 1 else rec(x - 1);System.out.println("""rec: (x: Int)Int""");$skip(13); val res$0 = 
		
		rec(24);System.out.println("""res0: Int = """ + $show(res$0))}
}