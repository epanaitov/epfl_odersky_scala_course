object zerochan_tables {
  
  val table = List(List(9, 4, 5, 5), List(5, 1, 3, 3), List(8, 1, 4, 5), List(8, 0, 5, 2))
                                                  //> table  : List[List[Int]] = List(List(9, 4, 5, 5), List(5, 1, 3, 3), List(8, 
                                                  //| 1, 4, 5), List(8, 0, 5, 2))
                                         
  
  case class Cell(val x: Int, val y: Int) {
  	override def toString: String = "(" + x + "," + y + ")"
  	lazy val square = x*y
  }
  object EmptyCell extends Cell(0, 0)
  object SingleArea extends Cell(1, 1)
  
	// build the m
	
	case class Matrix(val v: IndexedSeq[IndexedSeq[Cell]]) {
		override def toString: String = {
			"\n" + (for (ye <- v) yield (for (xe <- ye) yield xe.toString).mkString(", ")).mkString("\n")
		}
	}
	
	def makeMatrix(v: IndexedSeq[IndexedSeq[Cell]]): Matrix = Matrix(v)
                                                  //> makeMatrix: (v: IndexedSeq[IndexedSeq[zerochan_tables.Cell]])zerochan_tables
                                                  //| .Matrix
	
	def matrixCell(i: Int, j: Int): Cell = {
		if (j == 0) {
			if (i == 0) SingleArea
			else if (table(j)(i) >= table(j)(i-1)) Cell(matrixCell(i-1, j).x+1, matrixCell(i-1, j).y)
			else SingleArea
		} else {
			if (i == 0) {
				if (table(j)(i) >= table(j-1)(i)) Cell(matrixCell(i, j-1).x, matrixCell(i, j-1).y+1)
				else SingleArea
			} else {
				if ((table(j)(i) < table(j-1)(i)) && (table(j)(i) < table(j)(i-1))) SingleArea
				else if ((table(j)(i) >= table(j-1)(i)) && (table(j)(i) >= table(j)(i-1))) {
					// it's either +1 +1 or two options horizontal and vertical - compare them
					val horiz = if (table(j-1)(i) >= table(j-1)(i-1)) Cell(matrixCell(i-1, j).x+1, matrixCell(i-1, j).y)
					else Cell(matrixCell(i-1, j).x+1, 1)
					val vert = if (table(j)(i-1) >= table(j-1)(i-1)) Cell(matrixCell(i, j-1).x, matrixCell(i, j-1).y+1)
					else Cell(1, matrixCell(i, j-1).y+1)
					if (horiz == vert) horiz
					else if (horiz.square > vert.square) horiz
					else vert
				} else if (table(j)(i) >= table(j-1)(i)) Cell(matrixCell(i, j-1).x, matrixCell(i, j-1).y+1)
				else if (table(j)(i) >= table(j)(i-1)) Cell(matrixCell(i-1, j).x+1, matrixCell(i-1, j).y)
				else EmptyCell
			}
		}
	}                                         //> matrixCell: (i: Int, j: Int)zerochan_tables.Cell
	
	val m = makeMatrix(for (j <- 0 until table.size) yield for (i <- 0 until table(j).size) yield matrixCell(i, j))
                                                  //> m  : zerochan_tables.Matrix = 
                                                  //| (1,1), (1,1), (2,1), (3,1)
                                                  //| (1,1), (1,1), (2,1), (3,1)
                                                  //| (1,2), (1,2), (2,2), (3,2)
                                                  //| (1,3), (1,1), (1,3), (1,1)
	
	def findMaxCell(currentMax: Cell, i: Int, j: Int, mi: Int, mj: Int): Tuple2[Tuple2[Int, Int], Cell] = {
		if (i == m.v(j).size-1) {
			if (j == m.v.size-1) {
			 	if (m.v(j)(i).square > currentMax.square) ((i, j), m.v(j)(i))
			 	else ((mi, mj), currentMax)
			} else {
			 	if (m.v(j)(i).square > currentMax.square) findMaxCell(m.v(j)(i), 0, j+1, i, j)
			 	else findMaxCell(currentMax, 0, j+1, mi, mj)
			}
		} else {
			if (m.v(j)(i).square > currentMax.square) findMaxCell(m.v(j)(i), i+1, j, i, j)
			else findMaxCell(currentMax, i+1, j, mi, mj)
		}
	}                                         //> findMaxCell: (currentMax: zerochan_tables.Cell, i: Int, j: Int, mi: Int, mj
                                                  //| : Int)((Int, Int), zerochan_tables.Cell)
	
	val (coords, max) = findMaxCell(EmptyCell, 0, 0, -1, -1)
                                                  //> coords  : (Int, Int) = (3,2)
                                                  //| max  : zerochan_tables.Cell = (3,2)
  
  (for (row <- table) yield row.slice (coords._1 - max.x + 1, coords._1 + 1)).toList.slice(coords._2 - max.y+1, coords._2+1)
                                                  //> res0: List[List[Int]] = List(List(1, 3, 3), List(1, 4, 5))
}