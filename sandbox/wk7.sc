object wk7 {
	
	def isPrime(n: Int): Boolean = (2 until n) forall (x => (n % x != 0))
                                                  //> isPrime: (n: Int)Boolean
	
	((1 to 1000).toStream filter isPrime)(3)  //> res0: Int = 5
	
	def from(n: Int): Stream[Int] = n #:: from(n+1)
                                                  //> from: (n: Int)Stream[Int]
	
	val nats = from(0)                        //> nats  : Stream[Int] = Stream(0, ?)
	val m4s = nats map (_*4)                  //> m4s  : scala.collection.immutable.Stream[Int] = Stream(0, ?)
	
	def sieve(s: Stream[Int]): Stream[Int] = s.head #:: sieve(s.tail filter (_ % s.head != 0))
                                                  //> sieve: (s: Stream[Int])Stream[Int]
                                                  
  sieve(nats.filter (_ > 1)).take(10).toList      //> res1: List[Int] = List(2, 3, 5, 7, 11, 13, 17, 19, 23, 29)
  
  def sqrtStream(x: Double): Stream[Double] = {
  	def improve(guess: Double) = (guess + x / guess) / 2
  	lazy val guesses: Stream[Double] = 1 #:: (guesses map improve)
  	guesses
  }                                               //> sqrtStream: (x: Double)Stream[Double]
  
  (sqrtStream(2) take 11).toList apply 10         //> res2: Double = 1.414213562373095
  
  def isGoodEnough(guess: Double, x: Double) = math.abs((guess * guess - x)/x) < 0.0001
                                                  //> isGoodEnough: (guess: Double, x: Double)Boolean
  
  (sqrtStream(2) filter (isGoodEnough(_, 2))).head//> res3: Double = 1.4142156862745097
	
  abstract class IntSet {
  	def contains(x: Int): Boolean
  	def incl(x: Int): IntSet
  	def union(other: IntSet): IntSet
  }
  
  case class NonEmpty(e: Int, l: IntSet, r: IntSet) extends IntSet {
  	def contains(x: Int): Boolean = {
  		if (x < e) l contains x
  		else if (x > e) r contains x
  		else true
  	}
  	def incl(x: Int): IntSet = {
  		if (x < e) NonEmpty(e, l incl x, r)
  		else if (x > e) NonEmpty(e, l, r incl x)
  		else this
  	}
  	def union(other: IntSet): IntSet = (l union (r union (other))) incl e
  }
  
  object Empty extends IntSet {
  	def contains(x: Int) = false
  	def incl(x: Int): IntSet = NonEmpty(x, Empty, Empty)
  	def union(other: IntSet) = other
  }
 	 
}