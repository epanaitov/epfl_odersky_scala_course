object wk4 {import scala.runtime.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(56); 
  println("Welcome to the Scala worksheet")
	import week4._;$skip(275); 
	
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
	};System.out.println("""nth: [T](index: Integer, list: week4.List[T])T""");$skip(1161); 
	
	val One = new Succ(Zero);System.out.println("""One  : wk4.Succ = """ + $show(One ));$skip(25); 
	val Two = new Succ(One);System.out.println("""Two  : wk4.Succ = """ + $show(Two ));$skip(11); val res$0 = 
	One + One;System.out.println("""res0: wk4.Nat = """ + $show(res$0));$skip(11); val res$1 = 
	One + Two;System.out.println("""res1: wk4.Nat = """ + $show(res$1));$skip(11); val res$2 = 
	Two - One;System.out.println("""res2: wk4.Nat = """ + $show(res$2));$skip(30); 
	
	val One2 = new Succ2(Zero);System.out.println("""One2  : wk4.Succ2 = """ + $show(One2 ));$skip(13); val res$3 = 
	One2 + One2;System.out.println("""res3: wk4.Succ2 = """ + $show(res$3));$skip(12); val res$4 = 
	One2 + One;System.out.println("""res4: wk4.Succ2 = """ + $show(res$4));$skip(12); val res$5 = 
	One2 - One
	
	class Empty extends IntSet {
		def isEmpty = true
		def head = throw new Error
		def left = throw new Error
		def right = throw new Error
	}
	
	class NonEmpty(val head: Int, val left: IntSet, val right: IntSet) extends IntSet {
		def isEmpty = false
	};System.out.println("""res5: wk4.Nat = """ + $show(res$5));$skip(311); 
	
	def f(xs: List[NonEmpty], x: Empty) = xs.prepend(x)
                                                  
                                                  
	trait Expr
	
	case class Number(n: Int) extends Expr
	case class Sum(e1: Expr, e2: Expr) extends Expr
	case class Prod(e1: Expr, e2: Expr) extends Expr
	case class Var(name: String) extends Expr;System.out.println("""f: (xs: week4.List[wk4.NonEmpty], x: wk4.Empty)week4.List[week4.IntSet]""");$skip(446); 
	
	def eval(e: Expr): Int = e match {
		case Number(n) => n
		case Sum(e1, e2) => eval(e1) + eval(e2)
		case Prod(e1, e2) => eval(e1) * eval(e2)
	};System.out.println("""eval: (e: wk4.Expr)Int""");$skip(725); 
	
	
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
	};System.out.println("""show: (e: wk4.Expr)String""");$skip(24); 
	
	val ten = Number(10);System.out.println("""ten  : wk4.Number = """ + $show(ten ));$skip(21); 
	val one = Number(1);System.out.println("""one  : wk4.Number = """ + $show(one ));$skip(30); 
	val tenplus1 = Sum(ten, one);System.out.println("""tenplus1  : wk4.Sum = """ + $show(tenplus1 ));$skip(13); val res$6 = 
	
	show(ten);System.out.println("""res6: String = """ + $show(res$6));$skip(16); val res$7 = 
	show(tenplus1);System.out.println("""res7: String = """ + $show(res$7));$skip(51); val res$8 = 
	
	show(Prod(Sum(Number(2), Var("x")), Number(3)));System.out.println("""res8: String = """ + $show(res$8));$skip(50); val res$9 = 
	
	show(Sum(Prod(Number(2), Var("x")), Var("y")));System.out.println("""res9: String = """ + $show(res$9));$skip(50); val res$10 = 
	
	show(Prod(Sum(Number(2), Var("x")), Var("y")));System.out.println("""res10: String = """ + $show(res$10))}
	
}