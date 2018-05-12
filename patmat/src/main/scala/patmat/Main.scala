package patmat

import patmat.Huffman

object Main {
  def main(args : Array[String]) : Unit = {
    
    val freqs = Huffman.times(Huffman.string2Chars("AAAAAAABBBCDEFGH")) 
    
	val sampleTree = Huffman.createCodeTree(Huffman.string2Chars("AAAAAAAAABBBCDEFGH"))
	
	val frenchBit = Huffman.encode(Huffman.frenchCode)(Huffman.string2Chars("huffmanestcool")) 
	
	val sampleCodeTree = Huffman.convert(sampleTree)
	
	println(Huffman.quickEncode(sampleTree)(Huffman.string2Chars("BAC")))
  }
	
}