object wk7 {import scala.runtime.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(85); 
	
	def isPrime(n: Int): Boolean = (2 until n) forall (x => (n % x != 0));System.out.println("""isPrime: (n: Int)Boolean""");$skip(44); val res$0 = 
	
	((1 to 1000).toStream filter isPrime)(3);System.out.println("""res0: Int = """ + $show(res$0));$skip(51); 
	
	def from(n: Int): Stream[Int] = n #:: from(n+1);System.out.println("""from: (n: Int)Stream[Int]""");$skip(22); 
	
	val nats = from(0);System.out.println("""nats  : Stream[Int] = """ + $show(nats ));$skip(26); 
	val m4s = nats map (_*4);System.out.println("""m4s  : scala.collection.immutable.Stream[Int] = """ + $show(m4s ));$skip(94); 
	
	def sieve(s: Stream[Int]): Stream[Int] = s.head #:: sieve(s.tail filter (_ % s.head != 0));System.out.println("""sieve: (s: Stream[Int])Stream[Int]""");$skip(96); val res$1 = 
                                                  
  sieve(nats.filter (_ > 1)).take(10).toList;System.out.println("""res1: List[Int] = """ + $show(res$1));$skip(188); 
  
  def sqrtStream(x: Double): Stream[Double] = {
  	def improve(guess: Double) = (guess + x / guess) / 2
  	lazy val guesses: Stream[Double] = 1 #:: (guesses map improve)
  	guesses
  };System.out.println("""sqrtStream: (x: Double)Stream[Double]""");$skip(45); val res$2 = 
  
  (sqrtStream(2) take 11).toList apply 10;System.out.println("""res2: Double = """ + $show(res$2));$skip(91); 
  
  def isGoodEnough(guess: Double, x: Double) = math.abs((guess * guess - x)/x) < 0.0001;System.out.println("""isGoodEnough: (guess: Double, x: Double)Boolean""");$skip(54); val res$3 = 
  
  (sqrtStream(2) filter (isGoodEnough(_, 2))).head;System.out.println("""res3: Double = """ + $show(res$3))}
	
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