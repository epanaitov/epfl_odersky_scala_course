object week1 {import scala.runtime.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(59); 

	def abs(x : Double) = if (x < 0) -x else x;System.out.println("""abs: (x: Double)Double""");$skip(412); 

	def sqrt(x : Double) = {

		def isGoodEnough(guess : Double, x : Double) = abs(guess * guess - x) / x < 0.001
		def improve(guess : Double, x : Double) = (guess + x / guess) / 2

		def sqrtIter(guess : Double, x : Double) : Double = {
			if (x == 0.0) 0.0
			else if (isGoodEnough(guess, x)) guess
			else sqrtIter(improve(guess, x), x)
		}

		if (x < 0.0) sqrtIter(1.0, abs(x)) + "i" else sqrtIter(1.0, x)
	};System.out.println("""sqrt: (x: Double)Any""");$skip(14); val res$0 = 

	sqrt(1e-20);System.out.println("""res0: Any = """ + $show(res$0));$skip(11); val res$1 = 
	sqrt(0.0);System.out.println("""res1: Any = """ + $show(res$1));$skip(12); val res$2 = 
	sqrt(1e50);System.out.println("""res2: Any = """ + $show(res$2));$skip(10); val res$3 = 
	sqrt(-4);System.out.println("""res3: Any = """ + $show(res$3));$skip(10); val res$4 = 
	sqrt(17);System.out.println("""res4: Any = """ + $show(res$4));$skip(10); val res$5 = 
	sqrt(16);System.out.println("""res5: Any = """ + $show(res$5));$skip(10); val res$6 = 
	sqrt(12);System.out.println("""res6: Any = """ + $show(res$6));$skip(28); 
	
	
	val tolerance = 0.0001;System.out.println("""tolerance  : Double = """ + $show(tolerance ));$skip(80); 
	
	def isCloseEnough(x: Double, y: Double) =
			abs( (x-y) / x) / x < tolerance;System.out.println("""isCloseEnough: (x: Double, y: Double)Boolean""");$skip(247); 
	
	def fixedPoint(f: Double => Double)(firstGuess: Double) = {
	
			def iterate(guess: Double): Double = {
					
					val next = f(guess)
					
					if (isCloseEnough(guess, next)) next
					else iterate(next)
					
			}
			iterate(firstGuess)
	};System.out.println("""fixedPoint: (f: Double => Double)(firstGuess: Double)Double""");$skip(31); val res$7 = 
	
	fixedPoint(x => 1 + x/2)(1);System.out.println("""res7: Double = """ + $show(res$7));$skip(58); 
	def sqrt2(x: Double) = fixedPoint(y => (y + x/y) / 2)(1);System.out.println("""sqrt2: (x: Double)Double""");$skip(12); val res$8 = 
	
	sqrt2(2);System.out.println("""res8: Double = """ + $show(res$8));$skip(68); 
	
	def averageDamp(f: Double => Double)(x: Double) = (x + f(x)) / 2;System.out.println("""averageDamp: (f: Double => Double)(x: Double)Double""");$skip(113); 
                                                  
  def sqrt3(x: Double) = fixedPoint(averageDamp(y => x/y))(1);System.out.println("""sqrt3: (x: Double)Double""");$skip(112); val res$9 = 
                                                  
                                                  
	sqrt3(2);System.out.println("""res9: Double = """ + $show(res$9))}
	
}