object wk5 {import scala.runtime.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(56); 
  println("Welcome to the Scala worksheet");$skip(223); 
  
  def flatten(xs: List[Any]): List[Any] = xs match {
		case List() => xs
		case List(x) => x match {
			case i: Int => List(i)
			case l: List[Any] => flatten(l)
		}
		case y :: ys => flatten(List(y)) ::: flatten(ys)
	};System.out.println("""flatten: (xs: List[Any])List[Any]""");$skip(53); val res$0 = 
	
	flatten(List(List(1, 1), 2, List(3, List(5, 8))));System.out.println("""res0: List[Any] = """ + $show(res$0));$skip(465); 

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
	};System.out.println("""mergeSort: [T](xs: List[T])(lt: (T, T) => Boolean)List[T]""");$skip(68); 
	
	
	val list: List[Int] = 5 :: 14 :: 11 :: 7 :: 1 :: 2 :: 3 :: Nil;System.out.println("""list  : List[Int] = """ + $show(list ));$skip(34); val res$1 = 
	mergeSort(list)((x, y) => x < y);System.out.println("""res1: List[Int] = """ + $show(res$1));$skip(177); 
	
	def pack[T](xs: List[T]): List[List[T]] = xs match {
		case Nil => Nil
		case x :: xs1 => {
			val (head, tail) = xs span (x1 => x1 == x)
			List(head) ::: pack(tail)
		}
	};System.out.println("""pack: [T](xs: List[T])List[List[T]]""");$skip(56); 
	
	
	val data = List("a", "a", "a", "b", "c", "c", "a");System.out.println("""data  : List[java.lang.String] = """ + $show(data ));$skip(14); val res$2 = 
	
	pack(data);System.out.println("""res2: List[List[java.lang.String]] = """ + $show(res$2));$skip(143); 
                                                  
	def encode[T](xs: List[T]): List[Pair[T, Int]] = pack(xs) map (ys => (ys.head, ys.length));System.out.println("""encode: [T](xs: List[T])List[(T, Int)]""");$skip(16); val res$3 = 
	
	encode(data);System.out.println("""res3: List[(java.lang.String, Int)] = """ + $show(res$3));$skip(47); val res$4 = 
	
	
	(0 :: list) reduceRight ((x, y) => x + y);System.out.println("""res4: Int = """ + $show(res$4));$skip(42); val res$5 = 
	(1 :: list) reduceLeft ((x, y) => x * y);System.out.println("""res5: Int = """ + $show(res$5));$skip(23); val res$6 = 
	
	list.fold(0)(_ + _);System.out.println("""res6: Int = """ + $show(res$6));$skip(38); 
	
	val diverse = List(1, 0, -1, 0, 2);System.out.println("""diverse  : List[Int] = """ + $show(diverse ));$skip(108); 
	
  def mapFun[T, U](xs: List[T], f: T => U): List[U] = (xs foldLeft List[U]())((x, y) => x ::: List(f(y)));System.out.println("""mapFun: [T, U](xs: List[T], f: T => U)List[U]""");$skip(90); val res$7 = 
                                                  
	mapFun[Int, Int](diverse, x => x * 2);System.out.println("""res7: List[Int] = """ + $show(res$7));$skip(27); val res$8 = 
	
	diverse.map(x => x * 2);System.out.println("""res8: List[Int] = """ + $show(res$8));$skip(74); 
	
	
	def lengthFun[T](xs: List[T]): Int = (xs foldRight 0)((x, y) => y+1);System.out.println("""lengthFun: [T](xs: List[T])Int""");$skip(72); val res$9 = 
                                                  
 	lengthFun(diverse);System.out.println("""res9: Int = """ + $show(res$9))}
}