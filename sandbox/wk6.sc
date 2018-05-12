object wk6 {
  val hw = "Hello World"                          //> hw  : java.lang.String = Hello World
  hw filter (c => c.isUpper)                      //> res0: String = HW
  
  1 to 10 by 3                                    //> res1: scala.collection.immutable.Range = Range(1, 4, 7, 10)

	val pairs = List(1, 2, 3) zip hw          //> pairs  : List[(Int, Char)] = List((1,H), (2,e), (3,l))
	hw flatMap(c => List('.', c))             //> res2: String = .H.e.l.l.o. .W.o.r.l.d
	
	(1 to 10) flatMap (x => (1 to 3) map (y => (x, y)))
                                                  //> res3: scala.collection.immutable.IndexedSeq[(Int, Int)] = Vector((1,1), (1,2
                                                  //| ), (1,3), (2,1), (2,2), (2,3), (3,1), (3,2), (3,3), (4,1), (4,2), (4,3), (5,
                                                  //| 1), (5,2), (5,3), (6,1), (6,2), (6,3), (7,1), (7,2), (7,3), (8,1), (8,2), (8
                                                  //| ,3), (9,1), (9,2), (9,3), (10,1), (10,2), (10,3))

	def isPrime(n: Int): Boolean = {
		(2 until n) forall (x => (n % x != 0))
	}                                         //> isPrime: (n: Int)Boolean
	
	case class Person(a: String, b: Int)
	var persons = List(Person("a", 0))        //> persons  : List[wk6.Person] = List(Person(a,0))
	for (p <- persons if p.b > 1) yield p     //> res4: List[wk6.Person] = List()
	
	val n = 7                                 //> n  : Int = 7
	
  for {
  	i <- 1 until n
  	j <- 1 until n
  	if isPrime(i+j)
  } yield (i, j)                                  //> res5: scala.collection.immutable.IndexedSeq[(Int, Int)] = Vector((1,1), (1,2
                                                  //| ), (1,4), (1,6), (2,1), (2,3), (2,5), (3,2), (3,4), (4,1), (4,3), (5,2), (5,
                                                  //| 6), (6,1), (6,5))
  
  def scalarProduct(xs: List[Double], ys: List[Double]) : Double = {
  	(for {
  		(x, y) <- xs zip ys
  	} yield x*y) sum
  }                                               //> scalarProduct: (xs: List[Double], ys: List[Double])Double
}