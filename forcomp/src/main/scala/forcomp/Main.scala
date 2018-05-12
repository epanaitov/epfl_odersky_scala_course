package forcomp

import forcomp.Anagrams

object Main {
  
  def main(args : Array[String]) : Unit = {
    
    println(Anagrams.sentenceAnagrams(List("Yes", "man")))
    
    /*
    val realCombs = Anagrams.realCombs(Anagrams.sentenceOccurrences(List("Yes", "man")))
    var noword = 0
    for (comb <- realCombs) {
      Anagrams.dictionaryByOccurrences.get(comb) match {
        case None => {
          noword = noword +1
        }
        case Some(word) => println(word)
      }
    }
    println("length: " + realCombs.size + " non words: " + noword)
    println
    
    val combs = Anagrams.combinations(Anagrams.sentenceOccurrences(List("Yes", "man")))
    noword = 0;
    for (comb <- combs) {
      Anagrams.dictionaryByOccurrences.get(comb) match {
        case None => {
          noword = noword +1 
        }
        case Some(word) => println(word)
      }
    }
    println("length: " + combs.size + " non words: " + noword)
    */
  }
  
}