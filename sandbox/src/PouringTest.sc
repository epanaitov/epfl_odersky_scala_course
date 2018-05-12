object PouringTest {
  
  val problem = new Pouring(Vector(4, 9))         //> problem  : Pouring = Pouring@52b2a2d8
  
  problem.moves                                   //> res0: scala.collection.immutable.IndexedSeq[Product with Serializable with Po
                                                  //| uringTest.problem.Move] = Vector(Empty(0), Empty(1), Fill(0), Fill(1), Pour(0
                                                  //| ,1), Pour(1,0))
  problem.pathSets take 3                         //> res1: scala.collection.immutable.Stream[Set[PouringTest.problem.Path]] = Str
                                                  //| eam(Set(--> Vector(0, 0)), ?)
 	
 	problem.solution(6)                       //> res2: Stream[PouringTest.problem.Path] = Stream(Fill(1) Pour(1,0) Empty(0) P
                                                  //| our(1,0) Empty(0) Pour(1,0) Fill(1) Pour(1,0)--> Vector(4, 6), ?)
}