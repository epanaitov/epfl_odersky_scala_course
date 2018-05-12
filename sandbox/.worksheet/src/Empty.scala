import week4._

object Empty extends IntSet {import scala.runtime.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(65); 
	def isEmpty = true;System.out.println("""isEmpty: => Boolean""");$skip(28); 
	def head = throw new Error;System.out.println("""head: => Nothing""");$skip(28); 
	def left = throw new Error;System.out.println("""left: => Nothing""");$skip(29); 
	def right = throw new Error;System.out.println("""right: => Nothing""")}
}

class NonEmpty(val head: Int, val left: IntSet, val right: IntSet) extends IntSet {
	def isEmpty = false
}

object wk4 {;$skip(168); 
  println("Welcome to the Scala worksheet");$skip(260); 
  
  def nth[T](index: Integer, list: List[T]): T = {
  	
  	def nth0[T](i: Integer, list: List[T]): T = {
  		if (list.isEmpty) throw new IndexOutOfBoundsException
  		else if (i == index) list.head
  		else nth0(i+1, list.tail)
  	}
  	
  	nth0(0, list)
  }
  
	abstract class Nat {
		def isZero: Boolean
		def predecessor: Nat
		def + (that: Nat): Nat
		def - (that: Nat): Nat
		override def toString: String = {
			def toString0(acc: Integer, n: Nat): String = {
				if (n.isZero) acc.toString()
				else toString0(acc+1, n.predecessor)
			}
			toString0(0, this)
		}
	}
	
	object Zero extends Nat {
		def isZero = true
		def predecessor: Nothing = throw new Error("0.predecessor")
		def +(that: Nat) = that
		def -(that: Nat) = if (that.isZero) this else throw new Error("negative")
	}
	
	class Succ(n: Nat) extends Nat {
		def isZero = false
		def predecessor: Nat = n
		
		def +(that: Nat) = {
			def plus(sum: Nat, a: Nat): Nat = {
				if (a.isZero) sum
				else plus(new Succ(sum), a.predecessor)
			}
			plus(this, that)
		}
		def -(that: Nat) = {
			def minus(acc: Nat, s: Nat): Nat = {
				if (s.isZero) acc
				else minus(acc.predecessor, s.predecessor)
			}
			minus(this, that)
		}
	}
	
	class Succ2(n: Nat) extends Nat {
		def isZero = false
		def predecessor = n
		def +(that: Nat) = new Succ2(n + that)
		def -(that: Nat) = if (that.isZero) this else n - that.predecessor
	};System.out.println("""nth: [T](index: Integer, list: List[T])T""");$skip(1161); 
	
	val One = new Succ(Zero);System.out.println("""One  : wk4.Succ = """ + $show(One ));$skip(25); 
	val Two = new Succ(One);System.out.println("""Two  : wk4.Succ = """ + $show(Two ));$skip(11); val res$0 = 
	One + One;System.out.println("""res0: wk4.Nat = """ + $show(res$0));$skip(11); val res$1 = 
	One + Two;System.out.println("""res1: wk4.Nat = """ + $show(res$1));$skip(11); val res$2 = 
	Two - One;System.out.println("""res2: wk4.Nat = """ + $show(res$2));$skip(30); 
	
	val One2 = new Succ2(Zero);System.out.println("""One2  : wk4.Succ2 = """ + $show(One2 ));$skip(13); val res$3 = 
	One2 + One2;System.out.println("""res3: wk4.Succ2 = """ + $show(res$3));$skip(12); val res$4 = 
	One2 + One;System.out.println("""res4: wk4.Succ2 = """ + $show(res$4));$skip(12); val res$5 = 
	One2 - One;System.out.println("""res5: wk4.Nat = """ + $show(res$5));$skip(65); 
	
	val a: Array[NonEmpty] = Array(new NonEmpty(1, Empty, Empty));System.out.println("""a  : Array[NonEmpty] = """ + $show(a ));$skip(41); 
	
	
	def f(xs: List[NonEmpty], x: Empty);System.out.println("""f: (xs: List[NonEmpty], x: <error>)Unit""")
}