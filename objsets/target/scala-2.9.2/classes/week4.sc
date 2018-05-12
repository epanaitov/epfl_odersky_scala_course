import objsets._


object week4 {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  def nth[T](index: Integer, list: List[T]): T = {
  	
  	def nth0[T](i: Integer, list: List[T]): T = {
  		if (list.isEmpty) throw new IndexOutOfBoundsException
  		else if (i == index) list.head
  		else nth0(i+1, list.tail)
  	}
  	
  	nth0(0, list)
  }                                               //> nth: [T](index: Integer, list: List[T])T
  
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
	}
	
	val One = new Succ(Zero)                  //> One  : week4.Succ = 1
	val Two = new Succ(One)                   //> Two  : week4.Succ = 2
	One + One                                 //> res0: week4.Nat = 2
	One + Two                                 //> res1: week4.Nat = 3
	Two - One                                 //> res2: week4.Nat = 1
	
	val One2 = new Succ2(Zero)                //> One2  : week4.Succ2 = 1
	One2 + One2                               //> res3: week4.Succ2 = 2
	One2 + One                                //> res4: week4.Succ2 = 2
	One2 - One                                //> res5: week4.Nat = 0
	
	val a: Array[NonEmpty] = Array(new NonEmpty(new Tweet("", "", 0), new Empty(), new Empty()))
                                                  //> a  : Array[objsets.NonEmpty] = Array(objsets.NonEmpty@86e293a)

	
}