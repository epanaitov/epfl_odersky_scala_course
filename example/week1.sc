object week1 {

	def abs(x : Double) = if (x < 0) -x else x//> abs: (x: Double)Double

	def sqrt(x : Double) = {

		def isGoodEnough(guess : Double, x : Double) = abs(guess * guess - x) / x < 0.001
		def improve(guess : Double, x : Double) = (guess + x / guess) / 2

		def sqrtIter(guess : Double, x : Double) : Double = {
			if (x == 0.0) 0.0
			else if (isGoodEnough(guess, x)) guess
			else sqrtIter(improve(guess, x), x)
		}

		if (x < 0.0) sqrtIter(1.0, abs(x)) + "i" else sqrtIter(1.0, x)
	}                                         //> sqrt: (x: Double)Any

	sqrt(1e-20)                               //> res0: Any = 1.0000021484861236E-10
	sqrt(0.0)                                 //> res1: Any = 0.0
	sqrt(1e50)                                //> res2: Any = 1.0000003807575104E25
	sqrt(-4)                                  //> res3: Any = 2.000609756097561i
	sqrt(17)                                  //> res4: Any = 4.123106716962795
	sqrt(16)                                  //> res5: Any = 4.000000636692939
	sqrt(12)                                  //> res6: Any = 3.464616186413269
	
	
	val tolerance = 0.0001                    //> tolerance  : Double = 1.0E-4
	
	def isCloseEnough(x: Double, y: Double) =
			abs( (x-y) / x) / x < tolerance
                                                  //> isCloseEnough: (x: Double, y: Double)Boolean
	
	def fixedPoint(f: Double => Double)(firstGuess: Double) = {
	
			def iterate(guess: Double): Double = {
					
					val next = f(guess)
					
					if (isCloseEnough(guess, next)) next
					else iterate(next)
					
			}
			iterate(firstGuess)
	}                                         //> fixedPoint: (f: Double => Double)(firstGuess: Double)Double
	
	fixedPoint(x => 1 + x/2)(1)               //> res7: Double = 1.999755859375
	def sqrt2(x: Double) = fixedPoint(y => (y + x/y) / 2)(1)
                                                  //> sqrt2: (x: Double)Double
	
	sqrt2(2)                                  //> res8: Double = 1.4142135623746899
	
	def averageDamp(f: Double => Double)(x: Double) = (x + f(x)) / 2
                                                  //> averageDamp: (f: Double => Double)(x: Double)Double
                                                  
  def sqrt3(x: Double) = fixedPoint(averageDamp(y => x/y))(1)
                                                  //> sqrt3: (x: Double)Double
                                                  
                                                  
	sqrt3(2)                                  //> res9: Double = 1.4142135623746899
	
}