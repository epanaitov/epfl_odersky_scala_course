object wk4 {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
	import week4._
	
  def nth[T](index: Integer, list: List[T]): T = {
  	
  	def nth0[T](i: Integer, list: List[T]): T = {
  		if (list.isEmpty) throw new IndexOutOfBoundsException
  		else if (i == index) list.head
  		else nth0(i+1, list.tail)
  	}
  	
  	nth0(0, list)
  }                                               //> nth: [T](index: Integer, list: week4.List[T])T
  
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
	
	val One = new Succ(Zero)                  //> One  : wk4.Succ = 1
	val Two = new Succ(One)                   //> Two  : wk4.Succ = 2
	One + One                                 //> res0: wk4.Nat = 2
	One + Two                                 //> res1: wk4.Nat = 3
	Two - One                                 //> res2: wk4.Nat = 1
	
	val One2 = new Succ2(Zero)                //> One2  : wk4.Succ2 = 1
	One2 + One2                               //> res3: wk4.Succ2 = 2
	One2 + One                                //> res4: wk4.Succ2 = 2
	One2 - One                                //> res5: wk4.Nat = 0
	
	class Empty extends IntSet {
		def isEmpty = true
		def head = throw new Error
		def left = throw new Error
		def right = throw new Error
	}
	
	class NonEmpty(val head: Int, val left: IntSet, val right: IntSet) extends IntSet {
		def isEmpty = false
	}
	
	def f(xs: List[NonEmpty], x: Empty) = xs.prepend(x)
                                                  //> f: (xs: week4.List[wk4.NonEmpty], x: wk4.Empty)week4.List[week4.IntSet]
                                                  
                                                  
	trait Expr
	
	case class Number(n: Int) extends Expr
	case class Sum(e1: Expr, e2: Expr) extends Expr
	case class Prod(e1: Expr, e2: Expr) extends Expr
	case class Var(name: String) extends Expr
	
	def eval(e: Expr): Int = e match {
		case Number(n) => n
		case Sum(e1, e2) => eval(e1) + eval(e2)
		case Prod(e1, e2) => eval(e1) * eval(e2)
	}                                         //> eval: (e: wk4.Expr)Int
	
	
	def show(e: Expr): String = e match {
		case Number(n) => n.toString
		case Sum(e1, e2) => show(e1) + " + " + show(e2)
		case Prod(e1, e2) => {
			e2 match {
				case Var(_) => {
					e1 match {
						case Number(n) => show(e1) + show(e2)
						case Sum(_, _) => "(" + show(e1) + ") * " + show(e2)
						case Prod(_, _) => "(" + show(e1) + ") * " + show(e2)
						case Var(_) => show(e1) + show(e2)
					}
				}
				case _ => {
					e1 match {
						case Number(n) => show(e1) + "*" + show(e2)
						case Sum(_, _) => "(" + show(e1) + ") * " + show(e2)
						case Prod(_, _) => "(" + show(e1) + ") * " + show(e2)
						case Var(_) => show(e1) + " * " + show(e2)
					}
				}
			}
		}
		case Var(name: String) => name
	}                                         //> show: (e: wk4.Expr)String
	
	val ten = Number(10)                      //> ten  : wk4.Number = Number(10)
	val one = Number(1)                       //> one  : wk4.Number = Number(1)
	val tenplus1 = Sum(ten, one)              //> tenplus1  : wk4.Sum = Sum(Number(10),Number(1))
	
	show(ten)                                 //> res6: String = 10
	show(tenplus1)                            //> res7: String = 10 + 1
	
	show(Prod(Sum(Number(2), Var("x")), Number(3)))
                                                  //> res8: String = (2 + x) * 3
	
	show(Sum(Prod(Number(2), Var("x")), Var("y")))
                                                  //> res9: String = 2x + y
	
	show(Prod(Sum(Number(2), Var("x")), Var("y")))
                                                  //> res10: String = (2 + x) * y
	
}