object PouringTest {import scala.runtime.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(65); 
  
  val problem = new Pouring(Vector(4, 9));System.out.println("""problem  : Pouring = """ + $show(problem ));$skip(19); val res$0 = 
  
  problem.moves;System.out.println("""res0: scala.collection.immutable.IndexedSeq[Product with Serializable with PouringTest.problem.Move] = """ + $show(res$0));$skip(26); val res$1 = 
  problem.pathSets take 3;System.out.println("""res1: scala.collection.immutable.Stream[Set[PouringTest.problem.Path]] = """ + $show(res$1));$skip(25); val res$2 = 
 	
 	problem.solution(6);System.out.println("""res2: Stream[PouringTest.problem.Path] = """ + $show(res$2))}
}