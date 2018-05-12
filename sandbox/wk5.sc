object wk5 {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  def flatten(xs: List[Any]): List[Any] = xs match {
		case List() => xs
		case List(x) => x match {
			case i: Int => List(i)
			case l: List[Any] => flatten(l)
		}
		case y :: ys => flatten(List(y)) ::: flatten(ys)
	}                                         //> flatten: (xs: List[Any])List[Any]
	
	flatten(List(List(1, 1), 2, List(3, List(5, 8))))
                                                  //> res0: List[Any] = List(1, 1, 2, 3, 5, 8)

	def mergeSort[T](xs: List[T])(lt: (T, T) => Boolean): List[T] =  {
		val n = xs.length/2
		if (n == 0) xs
		else {
			def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
				case (Nil, Nil) => Nil
				case (x :: xs1, Nil) => xs
				case (Nil, y :: ys1) => ys
				case (x :: xs1, y :: ys1) => if (lt(x, y)) x :: merge(xs1, ys) else y :: merge(xs, ys1)
			}
			val (left, right) = xs splitAt n
			merge(mergeSort(left)(lt), mergeSort(right)(lt))
		}
	}                                         //> mergeSort: [T](xs: List[T])(lt: (T, T) => Boolean)List[T]
	
	
	val list: List[Int] = 5 :: 14 :: 11 :: 7 :: 1 :: 2 :: 3 :: Nil
                                                  //> list  : List[Int] = List(5, 14, 11, 7, 1, 2, 3)
	mergeSort(list)((x, y) => x < y)          //> res1: List[Int] = List(1, 2, 3, 5, 7, 11, 14)
	
	def pack[T](xs: List[T]): List[List[T]] = xs match {
		case Nil => Nil
		case x :: xs1 => {
			val (head, tail) = xs span (x1 => x1 == x)
			List(head) ::: pack(tail)
		}
	}                                         //> pack: [T](xs: List[T])List[List[T]]
	
	
	val data = List("a", "a", "a", "b", "c", "c", "a")
                                                  //> data  : List[java.lang.String] = List(a, a, a, b, c, c, a)
	
	pack(data)                                //> res2: List[List[java.lang.String]] = List(List(a, a, a), List(b), List(c, c
                                                  //| ), List(a))
                                                  
	def encode[T](xs: List[T]): List[Pair[T, Int]] = pack(xs) map (ys => (ys.head, ys.length))
                                                  //> encode: [T](xs: List[T])List[(T, Int)]
	
	encode(data)                              //> res3: List[(java.lang.String, Int)] = List((a,3), (b,1), (c,2), (a,1))
	
	
	(0 :: list) reduceRight ((x, y) => x + y) //> res4: Int = 43
	(1 :: list) reduceLeft ((x, y) => x * y)  //> res5: Int = 32340
	
	list.fold(0)(_ + _)                       //> res6: Int = 43
	
	val diverse = List(1, 0, -1, 0, 2)        //> diverse  : List[Int] = List(1, 0, -1, 0, 2)
	
  def mapFun[T, U](xs: List[T], f: T => U): List[U] = (xs foldLeft List[U]())((x, y) => x ::: List(f(y)))
                                                  //> mapFun: [T, U](xs: List[T], f: T => U)List[U]
                                                  
	mapFun[Int, Int](diverse, x => x * 2)     //> res7: List[Int] = List(2, 0, -2, 0, 4)
	
	diverse.map(x => x * 2)                   //> res8: List[Int] = List(2, 0, -2, 0, 4)
	
	
	def lengthFun[T](xs: List[T]): Int = (xs foldRight 0)((x, y) => y+1)
                                                  //> lengthFun: [T](xs: List[T])Int
                                                  
 	lengthFun(diverse)                        //> res9: Int = 5
}