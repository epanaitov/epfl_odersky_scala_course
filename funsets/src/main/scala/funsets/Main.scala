package funsets

object Main extends App {
  import FunSets._
  println(contains(singletonSet(1), 1))
  
  val union12 = union(singletonSet(1), singletonSet(2))
  println(contains(union12, 2))
  
  val union23 = union(singletonSet(3), singletonSet(2))
  val isect1223 = intersect(union12, union23)
  
  println(contains(isect1223, 2))
  
  def eleven(x: Int): Boolean = x == 11
  val union1011 = union(singletonSet(10), singletonSet(11))
  println(contains(filter(union1011, eleven), 11))
  
  def set_1010(x: Int) : Boolean = (x < 10) && (x > -10) 
  
  println(forall(set_1010, x => (x < 10) && (x > -10)))
  
  println(exists(set_1010, x => x == -10))
  
  printSet(map(set_1010, x => x*2))
}
