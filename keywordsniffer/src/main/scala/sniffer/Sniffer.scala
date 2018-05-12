package sniffer

import java.io.ByteArrayOutputStream
import java.nio.charset.Charset
import dispatch._

object Sniffer {
	case class ShortJob(title: String, url: String) {
  	  override def toString() = title + " [" + url + "]" 
  	}
  
	def sniffJobs(jurl: String): Stream[ShortJob] = {
  	  
  		val svc = url(jurl)
	
		val output = Http(svc.GET)
		val outputStream = output().getResponseBodyAsStream()
		
		val bytes:Stream[Byte] = Stream.continually(outputStream.read()).takeWhile(_ != -1).map(_.toByte)
		
		val baos = new ByteArrayOutputStream()
  		val zero: Stream[ShortJob] = Stream()
  		bytes.foldLeft(zero)((jobList, b) => {
  		  if (b == 10) {
		    // new line
		    val line = baos.toString("UTF-8")
		    baos.reset
		    
		    val jobPattern = """<h2><a.*href="([^"]+)".*>([^<]+)</a></h2>""".r
		    val nextPagePattern = """.*<a.*href="([^"]+/jobs/page/\d+/)".*>.*Older.*</a>.*""".r
		    line.trim() match {
		      case jobPattern(link, title) => ShortJob(title, link) #:: jobList
		      case nextPagePattern(link) => sniffJobs(link) #::: jobList
		      case _ => jobList 
		    }
		    
		  } else {
			baos.write(b)
			jobList
		  }
  		})
  	}
	
  	val jobs = sniffJobs("http://www.griddynamics.com/jobs/")
  	
  	val storyContentStartMarker = "<storycontent start>"
  	  
  	for (job <- jobs) {
  	  
  		println(job.title)
  	  
  		val request = url(job.url)
  		val output = Http(request.GET)
		val outputStream = output().getResponseBodyAsStream()
		
		val bytes:Stream[Byte] = Stream.continually(outputStream.read()).takeWhile(_ != -1).map(_.toByte)
		
		val baos = new ByteArrayOutputStream()
  		val storycontent = new ByteArrayOutputStream()
  	  
  	  	for (b <- bytes) {
  	  	  if (b == 10) {
  	  	    val line = baos.toString("UTF-8")
		    baos.reset
		    
		    val storyContentStartPattern = """<div.*class="storycontent">""".r
		    val storyContentEndPattern = """</div>""".r
		    
		    line.trim match {
  	  	      case storyContentStartPattern() => {
  	  	        storycontent.reset
  	  	        storycontent.write(storyContentStartMarker.toCharArray().map(_.toByte))
  	  	      }
  	  	      case storyContentEndPattern() => {
  	  	        val divcontent = storycontent.toString("UTF-8")
  	  	        if (divcontent.contains(storyContentStartMarker)) {
  	  	          println(divcontent)
  	  	          println
  	  	        }
  	  	        storycontent.reset
  	  	      }
  	  	      case _ => {}
  	  	    }
  	  	  } else {
  	  	    baos.write(b)
  	  	    storycontent.write(b)
  	  	  }
  	  	}
  	}
}