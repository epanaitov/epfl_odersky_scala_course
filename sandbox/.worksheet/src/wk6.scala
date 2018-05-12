object wk6 {import scala.runtime.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(37); 
  val hw = "Hello World";System.out.println("""hw  : java.lang.String = """ + $show(hw ));$skip(29); val res$0 = 
  hw filter (c => c.isUpper);System.out.println("""res0: String = """ + $show(res$0));$skip(18); val res$1 = 
  
  1 to 10 by 3;System.out.println("""res1: scala.collection.immutable.Range = """ + $show(res$1));$skip(35); 

	val pairs = List(1, 2, 3) zip hw;System.out.println("""pairs  : List[(Int, Char)] = """ + $show(pairs ));$skip(31); val res$2 = 
	hw flatMap(c => List('.', c));System.out.println("""res2: String = """ + $show(res$2));$skip(55); val res$3 = 
	
	(1 to 10) flatMap (x => (1 to 3) map (y => (x, y)));System.out.println("""res3: scala.collection.immutable.IndexedSeq[(Int, Int)] = """ + $show(res$3));$skip(79); 

	def isPrime(n: Int): Boolean = {
		(2 until n) forall (x => (n % x != 0))
	}
	
	case class Person(a: String, b: Int);System.out.println("""isPrime: (n: Int)Boolean""");$skip(76); 
	var persons = List(Person("a", 0));System.out.println("""persons  : List[wk6.Person] = """ + $show(persons ));$skip(39); val res$4 = 
	for (p <- persons if p.b > 1) yield p;System.out.println("""res4: List[wk6.Person] = """ + $show(res$4));$skip(13); 
	
	val n = 7;System.out.println("""n  : Int = """ + $show(n ));$skip(82); val res$5 = 
	
  for {
  	i <- 1 until n
  	j <- 1 until n
  	if isPrime(i+j)
  } yield (i, j);System.out.println("""res5: scala.collection.immutable.IndexedSeq[(Int, Int)] = """ + $show(res$5));$skip(130); 
  
  def scalarProduct(xs: List[Double], ys: List[Double]) : Double = {
  	(for {
  		(x, y) <- xs zip ys
  	} yield x*y) sum
  };System.out.println("""scalarProduct: (xs: List[Double], ys: List[Double])Double""")}
}