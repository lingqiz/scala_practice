package patmat

import common._

object Huffman {

 
  abstract class CodeTree
  case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree
  case class Leaf(char: Char, weight: Int) extends CodeTree



  // Part 1: Basics

  def weight(tree: CodeTree): Int = tree match
  {
    case Fork(left, right, chars, weight) => weight
    case Leaf(char, weight) 			  => weight
  }

  def chars(tree: CodeTree): List[Char] = tree match
  {  
    case Fork(left, right, chars, weight) => chars
    case Leaf(char, weight)				  => char :: Nil
  }
  
  def makeCodeTree(left: CodeTree, right: CodeTree) =
    Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right))



  // Part 2: Generating Huffman trees

  def string2Chars(str: String): List[Char] = str.toList

 
  def mergeSort[T](list: List[T])(cmp: (T, T) => Boolean): List[T] = 
  {
    val middle = list.length / 2
    if(middle <= 0) list
    else 
    {
      def merge(left: List[T], right: List[T], acc: List[T]): List[T] = (left, right) match
      {
        case (Nil, right) => acc.reverse ::: right
        case (left, Nil)  => acc.reverse ::: left
        case (x :: xs, y :: ys) =>
    	    if	(cmp(x, y))  merge(xs, right, x :: acc)
    	    else             merge(left, ys,  y :: acc) 
      }
      
      val (left, right) = list splitAt middle
      merge(mergeSort(left)(cmp), mergeSort(right)(cmp), Nil)
    }
  }
  
  def map[T, U](list: List[T])(m: T => U): List[U] = list match
  {
    case Nil => Nil
    case x :: xs => m(x) :: map(xs)(m)
  }
  
  def encode[T](list: List[T]): List[(T, Int)] = list match
  {
    case Nil => Nil
    case x :: xs => 
      {
    	  val (head, tail) = list span(y => y == x)
    	  (head.head, head.length) :: encode(tail)
      }
      
  }
  
  

  def times(chars: List[Char]): List[(Char, Int)] = 
    encode( mergeSort(chars)(_ <= _) )

  
    //Map function!		Generic Map Function 
  def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] = 
  {
    val ordered = mergeSort(freqs)(_._2 <= _._2)
    map(ordered)(pair => Leaf(pair._1, pair._2))
   
  }
  
 
  def singleton(trees: List[CodeTree]): Boolean = trees match 
  {
    case tree :: Nil => true
    case _			 => false
  }


  def combine(trees: List[CodeTree]): List[CodeTree] = 
  {
    def insert(tree: CodeTree, list: List[CodeTree]): List[CodeTree] = list match
    {
      case Nil => tree :: Nil
      case x :: xs =>
        {
        	if(weight(x) >= weight(tree)) tree :: list 
        	else 					      x ::insert(tree, xs) 
        }
    }
    
    trees match 
    {
      case x :: y :: remain => insert(makeCodeTree(x, y), remain)
      case _				=> trees
    }
  }

  def until(end: List[CodeTree] => Boolean, combine: List[CodeTree] => List[CodeTree])(trees: List[CodeTree]): List[CodeTree] = 
    if(end(trees)) trees else until(end, combine)(combine(trees))

  def createCodeTree(chars: List[Char]): CodeTree = 
    until(singleton, combine)(makeOrderedLeafList(times(chars))).head



  // Part 3: Decoding

  type Bit = Int

  def decode(tree: CodeTree, bits: List[Bit]): List[Char] = 
  {
      def single(current: CodeTree, codes: List[Bit]): (Char, List[Bit]) = (current, codes) match
      {
        case (Leaf(char, _), _) => (char, codes)
        case (Fork(left, right, _, _), code :: remain) =>
          {
            if(code == 0) single(left,  remain)
            else		  single(right, remain)
          }
        case (_, _) => throw new Error("Decode Problem")
      }
      
      bits match
      {
        case Nil => Nil
        case xs  => 
          val (char, remain) = single(tree, xs)
          char :: decode(tree, remain)
      }
  }

 
  val frenchCode: CodeTree = Fork(Fork(Fork(Leaf('s',121895),Fork(Leaf('d',56269),Fork(Fork(Fork(Leaf('x',5928),Leaf('j',8351),List('x','j'),14279),Leaf('f',16351),List('x','j','f'),30630),Fork(Fork(Fork(Fork(Leaf('z',2093),Fork(Leaf('k',745),Leaf('w',1747),List('k','w'),2492),List('z','k','w'),4585),Leaf('y',4725),List('z','k','w','y'),9310),Leaf('h',11298),List('z','k','w','y','h'),20608),Leaf('q',20889),List('z','k','w','y','h','q'),41497),List('x','j','f','z','k','w','y','h','q'),72127),List('d','x','j','f','z','k','w','y','h','q'),128396),List('s','d','x','j','f','z','k','w','y','h','q'),250291),Fork(Fork(Leaf('o',82762),Leaf('l',83668),List('o','l'),166430),Fork(Fork(Leaf('m',45521),Leaf('p',46335),List('m','p'),91856),Leaf('u',96785),List('m','p','u'),188641),List('o','l','m','p','u'),355071),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u'),605362),Fork(Fork(Fork(Leaf('r',100500),Fork(Leaf('c',50003),Fork(Leaf('v',24975),Fork(Leaf('g',13288),Leaf('b',13822),List('g','b'),27110),List('v','g','b'),52085),List('c','v','g','b'),102088),List('r','c','v','g','b'),202588),Fork(Leaf('n',108812),Leaf('t',111103),List('n','t'),219915),List('r','c','v','g','b','n','t'),422503),Fork(Leaf('e',225947),Fork(Leaf('i',115465),Leaf('a',117110),List('i','a'),232575),List('e','i','a'),458522),List('r','c','v','g','b','n','t','e','i','a'),881025),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u','r','c','v','g','b','n','t','e','i','a'),1486387)

  val secret: List[Bit] = List(0,0,1,1,1,0,1,0,1,1,1,0,0,1,1,0,1,0,0,1,1,0,1,0,1,1,0,0,1,1,1,1,1,0,1,0,1,1,0,0,0,0,1,0,1,1,1,0,0,1,0,0,1,0,0,0,1,0,0,0,1,0,1)

  def decodedSecret: List[Char] = decode(frenchCode, secret)



  // Part 4a: Encoding using Huffman tree

  def encode(tree: CodeTree)(text: List[Char]): List[Bit] =  
  {
    def sCode(cTree: CodeTree, char: Char): List[Bit] = cTree match 
    {
      case Leaf(_, _) 				=> Nil
      case Fork(left, right, _, _)  => 
        if(chars(left) contains char) 0 :: sCode(left,  char)
        else						  1 :: sCode(right, char)
    }
    
    def tailHelper(text: List[Char])(res: List[Bit]): List[Bit] = text match
    {
      case Nil => res
      case c :: cs =>  
        {
          println(cs.length)
          tailHelper(cs)(res ::: sCode(tree, c))
        }
    }
    
    tailHelper(text)(Nil)
  }


  // Part 4b: Encoding using code table

  type CodeTable = List[(Char, List[Bit])]
  abstract class CodeBinaryTree
  {
	  def add(e: (Char, List[Bit])): CodeBinaryTree = this match
	  {
	    case Empty => Node(Empty, Empty, e)
	    case Node(left, right, (c, b)) =>
	      {
	        if      (e._1 < c) Node(left.add(e), right, (c, b))
	        else if (e._1 > c) Node(left, right.add(e), (c, b))
	        else Node(left, right, e)
	      }
	  }
	  
	  def get(key: Char): List[Bit] = this match
	  {
	    case Empty => throw new NoSuchElementException()
	    case Node(left, right, (c, b)) =>
	      {
	        if      (key < c) left. get(key)
	        else if (key > c) right.get(key)
	        else b
	      }
	  }
	  
	  override def toString: String = this match
	  {
	    case Empty => "."
	    case Node(left, right, (c, _)) => s"($left$c$right)"
	  }
  }
  
  case class  Node(left: CodeBinaryTree, right: CodeBinaryTree, code: (Char, List[Bit])) extends CodeBinaryTree
  case object Empty extends CodeBinaryTree 

  def codeBits(tree: CodeBinaryTree)(char: Char): List[Bit] = tree.get(char) 
  
 
  def convert(tree: CodeTree): CodeTable = 
  {
	def helper(cTree: CodeTree, bits: List[Bit]): CodeTable = cTree match
	{
	  case Leaf(char, weight) 	    => (char, bits.reverse) :: Nil
	  case Fork(left, right, _, _)  => 
	    helper(left, 0 :: bits) ::: helper(right, 1 :: bits)
	}
	
	helper(tree, Nil)
  }
  
  def searchTree(flat: CodeTable)(tree: CodeBinaryTree): CodeBinaryTree = flat match
  {
    case Nil     => tree
    case f :: fs => searchTree(fs)(tree.add(f))
  }
  
  def convert2SearchTree(tree: CodeTree): CodeBinaryTree = searchTree(convert(tree))(Empty)


  def quickEncode(tree: CodeTree)(text: List[Char]): List[Bit] = 
  {
    val searchTree = convert2SearchTree(tree)

    def helper(chars: List[Char], res: List[Bit]): List[Bit] = chars match
    {
    	case Nil 	 => res
    	case c :: cs => 
    	  {
    	    println(cs.length)
    	    helper(cs, res ::: codeBits(searchTree)(c))
    	  }
    }
  	
    helper(text, Nil)
  }
}
