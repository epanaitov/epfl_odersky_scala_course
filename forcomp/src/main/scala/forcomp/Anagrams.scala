package forcomp

import common._

object Anagrams {

  /** A word is simply a `String`. */
  type Word = String

  /** A sentence is a `List` of words. */
  type Sentence = List[Word]

  /** `Occurrences` is a `List` of pairs of characters and positive integers saying
   *  how often the character appears.
   *  This list is sorted alphabetically w.r.t. to the character in each pair.
   *  All characters in the occurrence list are lowercase.
   *  
   *  Any list of pairs of lowercase characters and their frequency which is not sorted
   *  is **not** an occurrence list.
   *  
   *  Note: If the frequency of some character is zero, then that character should not be
   *  in the list.
   */
  type Occurrences = List[(Char, Int)]

  /** The dictionary is simply a sequence of words.
   *  It is predefined and obtained as a sequence using the utility method `loadDictionary`.
   */
  val dictionary: List[Word] = loadDictionary

  /** Converts the word into its character occurence list.
   *  
   *  Note: the uppercase and lowercase version of the character are treated as the
   *  same character, and are represented as a lowercase character in the occurrence list.
   */
  def wordOccurrences(w: Word): Occurrences = w.groupBy((c: Char) => c.toLower).map(pair => (pair._1, pair._2.length)).toList.sortWith(_._1 < _._1)

  /** Converts a sentence into its character occurrence list. */
  def sentenceOccurrences(s: Sentence): Occurrences = wordOccurrences(s.mkString)

  /** The `dictionaryByOccurrences` is a `Map` from different occurrences to a sequence of all
   *  the words that have that occurrence count.
   *  This map serves as an easy way to obtain all the anagrams of a word given its occurrence list.
   *  
   *  For example, the word "eat" has the following character occurrence list:
   *
   *     `List(('a', 1), ('e', 1), ('t', 1))`
   *
   *  Incidentally, so do the words "ate" and "tea".
   *
   *  This means that the `dictionaryByOccurrences` map will contain an entry:
   *
   *    List(('a', 1), ('e', 1), ('t', 1)) -> Seq("ate", "eat", "tea")
   *
   */
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = dictionary.groupBy(w => wordOccurrences(w))

  /** Returns all the anagrams of a given word. */
  def wordAnagrams(word: Word): List[Word] = dictionaryByOccurrences(wordOccurrences(word))  

  /** Returns the list of all subsets of the occurrence list.
   *  This includes the occurrence itself, i.e. `List(('k', 1), ('o', 1))`
   *  is a subset of `List(('k', 1), ('o', 1))`.
   *  It also include the empty subset `List()`.
   * 
   *  Example: the subsets of the occurrence list `List(('a', 2), ('b', 2))` are:
   *
   *    List(
   *      List(),
   *      List(('a', 1)),
   *      List(('a', 2)),
   *      List(('b', 1)),
   *      List(('a', 1), ('b', 1)),
   *      List(('a', 2), ('b', 1)),
   *      List(('b', 2)),
   *      List(('a', 1), ('b', 2)),
   *      List(('a', 2), ('b', 2))
   *    )
   *
   *  Note that the order of the occurrence list subsets does not matter -- the subsets
   *  in the example above could have been displayed in some other order.
   */
  def combinations(occurrences: Occurrences): List[Occurrences] = {
    
    def listCombs(occs: Occurrences): List[Occurrences] = {
      
      /** modifies the list of Occurrences to decrease the element by one */ 
      def decrease(ox: Occurrences, occ : Pair[Char, Int]): Occurrences = {
        if (occ._2 == 1) ox.filterNot(x => x == occ)
        else ox.updated(ox.indexOf(occ), (occ._1, occ._2-1))
      }
      
      if (occs.isEmpty) occs :: List.empty
      else {
        val mods = for (o <- occs) yield listCombs(decrease(occs, o))
        occs :: mods.flatten
      }
    }
    
    listCombs(occurrences).toSet.toList
  }
  
  /** same as combinations but does not produce occurrences which are not in the dictionary */
  def realCombs(occurrences: Occurrences): List[Occurrences] = {
    
    def listCombs(occs: Occurrences): List[Occurrences] = {
      
      /** modifies the list of Occurrences to decrease the element by one */ 
      def decrease(ox: Occurrences, occ : Pair[Char, Int]): Occurrences = {
        if (occ._2 == 1) ox.filterNot(x => x == occ)
        else ox.updated(ox.indexOf(occ), (occ._1, occ._2-1))
      }
      
      if (occs.isEmpty) occs :: List.empty
      else {
        val mods = for {
          o <- occs
          val dec = decrease(occs, o)
          val tail = listCombs(dec)
          t <- tail
          if (dictionaryByOccurrences.get(t) != None)
          } yield t
        if (dictionaryByOccurrences.get(occs) != None) occs :: mods
        else mods
      }
    }
    
    listCombs(occurrences).toSet.toList
    
  }

  /** Subtracts occurrence list `y` from occurrence list `x`.
   * 
   *  The precondition is that the occurrence list `y` is a subset of
   *  the occurrence list `x` -- any character appearing in `y` must
   *  appear in `x`, and its frequency in `y` must be smaller or equal
   *  than its frequency in `x`.
   *
   *  Note: the resulting value is an occurrence - meaning it is sorted
   *  and has no zero-entries.
   */
  def subtract(x: Occurrences, y: Occurrences): Occurrences = {
    val ym = y groupBy(p => p._1)
    val zero: List[(Char, Int)] = List.empty
    val diff = (x.foldLeft(zero))((xs, x) => if (ym.get(x._1) == None) x :: xs else {
      val diff = x._2 - ym(x._1).head._2
      if (diff == 0) xs
      else (x._1, diff) :: xs
      })
    diff.sortWith(_._1 < _._1)
  }

  /** Returns a list of all anagram sentences of the given sentence.
   *  
   *  An anagram of a sentence is formed by taking the occurrences of all the characters of
   *  all the words in the sentence, and producing all possible combinations of words with those characters,
   *  such that the words have to be from the dictionary.
   *
   *  The number of words in the sentence and its anagrams does not have to correspond.
   *  For example, the sentence `List("I", "love", "you")` is an anagram of the sentence `List("You", "olive")`.
   *
   *  Also, two sentences with the same words but in a different order are considered two different anagrams.
   *  For example, sentences `List("You", "olive")` and `List("olive", "you")` are different anagrams of
   *  `List("I", "love", "you")`.
   *  
   *  Here is a full example of a sentence `List("Yes", "man")` and its anagrams for our dictionary:
   *
   *    List(
   *      List(en, as, my), v
   *      List(en, my, as), v
   *      List(man, yes),   v
   *      List(men, say),   v
   *      List(as, en, my), v
   *      List(as, my, en),
   *      List(sane, my),   v 
   *      List(Sean, my),   v
   *      List(my, en, as),
   *      List(my, as, en),
   *      List(my, sane),
   *      List(my, Sean),
   *      List(say, men),   v
   *      List(yes, man)    v
   *    )
   *
   *  The different sentences do not have to be output in the order shown above - any order is fine as long as
   *  all the anagrams are there. Every returned word has to exist in the dictionary.
   *  
   *  Note: in case that the words of the sentence are in the dictionary, then the sentence is the anagram of itself,
   *  so it has to be returned in this list.
   *
   *  Note: There is only one anagram of an empty sentence.
   */
  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
    if (sentence.isEmpty) List(List())
    else {
      
      def sa1(soccs: Occurrences): List[List[Occurrences]] = {
        soccs match {
          case List() => List(List())
          case _ => {
            val combs = realCombs(soccs)
            for {
	            comb <- combs
	            t <- sa1(subtract(soccs, comb))
	          } yield comb :: t
          }
        }
      }
      
      def sa2(loccs: List[Occurrences]): List[Sentence] = {
        loccs match {
          case List() => List(List())
          case head :: tail => {
            for {
	            word <- dictionaryByOccurrences(head)
	            tail <- sa2(tail) 
	          } yield word :: tail
          }
        }
      }
      
      val zero: List[List[Sentence]] = List(List())
      (sa1(sentenceOccurrences(sentence)).filterNot(el => el.isEmpty).foldLeft(zero))((acc, el) => sa2(el) :: acc).flatten.toSet.toList
    }
  }

}
