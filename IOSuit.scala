package patmat

import java.io.FileReader
import java.io.File

object IOSuit 
{
	def main(args: Array[String]): Unit = 
	{
	  val content  = readFile("Old Mortality.txt")
	  val codeTree = Huffman.createCodeTree(content)
	  val bits = Huffman.quickEncode(codeTree)(content)
	  println(bits.length)
	  println(convert(bits))
	}
	
	def convert(list: List[Int]): List[Int] = 
	{
	  if(list.length > 32)
	  {
	    val bits = list.take(32).map(x => (x + 48).asInstanceOf[Char]).toArray
	    println(java.util.Arrays.toString(bits))
	    val str = new String(bits)
	    Integer.parseInt(str, 2) :: convert(list.drop(32))
	  }
		    
	    
	  else if(list.length > 0) 
	    Integer.parseInt(new String(list.map(x => x.asInstanceOf[Char]).toArray), 2) :: Nil
	  
	  else Nil 
	}
	
	
	
	def readFile(path: String): List[Char] = 
	{
	  def file = new File(path)
	  val chars = new Array[Char](file.length.asInstanceOf[Int])
	  val read  = new FileReader(path).read(chars)

	  new String(chars).toList
	}
}