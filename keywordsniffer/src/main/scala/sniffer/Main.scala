package sniffer

import java.io.File

object Main extends App {
  
  def subFile(file: File, children: String*) = {
    children.foldLeft(file)((file, child) => new File(file, child))
  }
  
  def resourceAsStreamFromSrc(resourcePath: List[String]): Option[java.io.InputStream] = {
    val classesDir = new File(getClass.getResource(".").toURI)
    val projectDir = classesDir.getParentFile.getParentFile.getParentFile.getParentFile
    val resourceFile = subFile(projectDir, ("src" :: "main" :: "resources" :: resourcePath): _*)
    if (resourceFile.exists)
      Some(new java.io.FileInputStream(resourceFile))
    else
      None
  }
  
  	def loadDictionary = {
	    val wordstream = Option {
	      getClass.getClassLoader.getResourceAsStream("gd-jobs.txt")
	    } orElse {
	      resourceAsStreamFromSrc(List("gd-jobs.txt"))
	    } getOrElse {
	      sys.error("Could not load word list, dictionary file not found")
	    }
	    try {
	      val s = io.Source.fromInputStream(wordstream)
	      s.getLines.toList
	    } catch {
	      case e: Exception =>
	        println("Could not load word list: " + e)
	        throw e
	    } finally {
	      wordstream.close()
	    }
	}
  
  def tagSplit(strings: List[String], index: Int): List[String] = {
    
    def singleTag(strings: List[String], tag: String): List[String] = (for (str <- strings) yield str.split(tag).map(_.trim)).flatten.filterNot(_.isEmpty())
    def doubleTag(strings: List[String], opening: String, closing: String): List[String] = singleTag(singleTag(strings, opening), closing)
    
      index match {
        case 0 => tagSplit(doubleTag(strings, "<storycontent start>", "</div>"), 1)
        case 1 => tagSplit(doubleTag(strings, "<strong[^>]*>", "</strong>"), 2)
        case 2 => tagSplit(doubleTag(strings, "<p[^>]*>", "</p>"), 3)
        case 3 => tagSplit(singleTag(strings, """<br\s*/?>"""), 4)
        case 4 => tagSplit(doubleTag(strings, "<ul[^>]*>", "</ul>"), 5)
        case 5 => tagSplit(doubleTag(strings, "<li[^>]*>", "</li>"), 6)
        case 6 => tagSplit(doubleTag(strings, "<h3[^>]*>", "</h3>"), 7)
        case 7 => tagSplit(singleTag(strings, "<div[^>]*>"), 8)
        case 8 => tagSplit(doubleTag(strings, "<span[^>]*>", "</span>"), 9)
        case 9 => strings
      }
  }
  
  val invariants = List(
      "San Francisco/Bay Area", 
      "Core Java", 
      "Enterprise Java", 
      "Grid Dynamics", 
      "San Francisco / Bay Area", 
      "Big Data", 
      "Machine learning",
      "data mining",
      "pattern mining",
      "Apache HBase",
      "Greenplum DB",
      "Sybase IQ",
      "Relational DB",
      "NoSQL DB",
      "IBM WebSphere",
      "Oracle Weblogic",
      "Apache Tomcat",
      "Cruise Control",
      "Scrum Master",
      "Daily Stand-Up",
      "Continuos Integration",
      "Continuos Delivery",
      "Continues Integration",
      "Continues Delivery",
      "Continuous Integration",
      "Continuous Delivery",
      "San Francisco",
      "load balancing",
      "JVM internals",
      "open source",
      "IBM InfoSphere Data Architect",
      "IBM Message Broker",
      "Service Orientation",
      "IBM MQ",
      "IBM Message Broker",
      "X.509 Authentication",
      "Digital Signature",
      "VMWare Cloud",
      "Service Oriented Architecture",
      "Web services",
      "problem solving",
      "CAP theorem",
      "high quality code",
      "excellent technical background",
      "Analysis Services",
      "SQL Server",
      "Data Warehouse Architecture",
      "Java platform",
      "JVM internals",
      "memory model",
      "test harness",
      "testing harness",
      "test harnesses",
      "testing harnesses"
      )
  
  def removeInvariant(str1: String, str2: String): String = {
    if (str1.toLowerCase().contains(str2)) {
      val substrPos = str1.toLowerCase().indexOf(str2) 
      removeInvariant(str1.take(substrPos) + str1.drop(substrPos + str2.length()), str2.toLowerCase()) 
    } else str1
  }
      
  def countInvariants(string: String): Map[String, Int] = {
    
    def ci0(string: String, invariants: List[String], counts: Map[String, Int]): Map[String, Int] = {
      
      def ci1(str1: String, str2: String, count: Int): Int = {
        if (str1.contains(str2)) {
          ci1(str1.replaceFirst(str2, ""), str2, count+1)
        } else count
      }
      
      
      if (invariants.isEmpty) counts
      else {
        val head = invariants.head
        val count = ci1(string.toLowerCase(), head.toLowerCase(), 0)
        if (count == 0) ci0(string, invariants.tail, counts)
        else {
	        val reduced = removeInvariant(string, head.toLowerCase())
	        ci0(reduced, invariants.tail, counts.+(head -> count))
        }
      }
    }
    ci0(string, invariants, Map())
  }
  
  def removeInvariants(string: String): String = {
    
    def ri0(string: String, invariants: List[String]): String = {
      if (invariants.isEmpty) string
      else {
        ri0(removeInvariant(string, invariants.head.toLowerCase()), invariants.tail)
      }
    }
    
    ri0(string, invariants)
  }
      
  val zero: Map[String, Int] = Map()
  val statsMap = loadDictionary.foldLeft(zero)((map0, line) => {
    val chunks = tagSplit(List(line), 0)
    chunks.foldLeft(map0)((map, chunk) => {
      
      val localCounts = countInvariants(chunk);
      val ichunk = removeInvariants(chunk);
      
      val imap = localCounts.foldLeft(map)((lmap, lc) => {
        if (lmap.contains(lc._1)) lmap.updated(lc._1, lmap(lc._1) + lc._2)
        else lmap + lc
      })
      
      val termPattern = """(\(|\s|^|\\A|")?([\.a-zA-Z0-9\-\+\\/\`\'\’\#\%]*[a-zA-Z0-9\-\+\\/\`\'\’\#\%]+)(\)|\s|\,|\.\s|\.$|$|\\z|\.\\z|\:|\;|")?""".r
      val terms = for (m <- termPattern.findAllIn(ichunk).matchData) yield m.group(2).toLowerCase()
      
      terms.foldLeft(imap)((lmap, term) => {
        if (lmap.contains(term)) lmap.updated(term, lmap(term) + 1)
        else lmap.+(term -> 1)
      })
    })
  })
  
  val statsList = statsMap.toList.sortWith((a, b) => a._2 > b._2)
  statsList.foreach(stat => println(stat._2 + ":" + stat._1))
}