package tree

object TreeStructure 
{
  /**
   * Search Tree property : 
   *    element to the left of the node  < the node 
   *    element to the right of the node > the node 
   *    
   *    Binary Search Tree as Dynamic Sorted Array 
   *     
   *    Red - Black (Balanced) Binary Search Tree 
   *      - Each node is RED / BLACK
   *      - Root node is BLACK 
   *      
   *      - *No two RED nodes in a row 
   *      - *Every root -> null path has exactly the SAME number of BLACK nodes
   *      
   *      Every Red - Black Tree with N nodes has height <= 2 log (N + 1) 
   *        - if every root - null path has at least K nodes, the tree includes a
   *        perfect balanced search tree of depth K - 1 
   *        
   *        - Red - Black Tree has balanced black tree structure 
   *            -> black nodes has depth = log N 
   *            -> worst case all interleaved red & black nodes : 2 * log N height worst case 
   *            -> guarantee to be log 
   *            
   *            *maintain the invariance 
   *              -> rotation : locally re-balance subtrees at O(1) time      
   */
  
  
  // Left Leaning Red - Black Balanced Binary Search Tree
  abstract class Tree[+A <% Ordered[A]]
  {
   // def parent: Tree[A]
    def left:   Tree[A]
    def right:  Tree[A]
    def elem:   A
    def size:   Int 
    def color:  Boolean
    
    private val RED   = true
    private val BLACK = false 
    
    def min: A = this match
    {
      case Leaf => throw new NoSuchElementException("Empty Tree")
      case Branch(Leaf, _, e, _) => e
      case Branch(l, _, _, _)    => l.min
    }
    
    def max: A = this match
    {
      case Leaf => throw new NoSuchElementException("Empty Tree")
      case Branch(_, Leaf, e, _) => e
      case Branch(_, r, _, _)    => r.max
    }
    
    
    def toList: List[A] = this match
    {      
      case Leaf => Nil
      case Branch(l, r, e, _) => l.toList ::: e :: r.toList
    }
    
    private def leftRotation[B >: A <% Ordered[B]](node: Tree[B]): Tree[B] = node match
    {
      case Branch(a, Branch(b, c, y, RED), x, cx) if a.color == BLACK =>
        Branch(Branch(a, b, x, RED), c, y, cx)
        
      case _ => node
    }
    
    private def rightRotation[B >: A <% Ordered[B]](node: Tree[B]): Tree[B] = node match
    {
      case Branch(Branch(a, b, y, RED), c, x, cx) if a.color == RED =>
        Branch(a, Branch(b, c, x, RED), y, cx)
        
      case _ => node
    }
    
    private def flipColor[B >: A <% Ordered[B]](node: Tree[B]): Tree[B] = node match
    {
      case Branch(Branch(ll, lr, le, RED), Branch(rl, rr, re, RED), e, _) =>
        Branch(Branch(ll, lr, le, BLACK), Branch(rl, rr, re, BLACK), e, RED)
        
      case _ => node
    }
    
    def insert[B >: A <% Ordered[B]](key: B): Tree[B] = this match
    {
      case Leaf => Branch(Leaf, Leaf, key, RED)
      case Branch(l, r, e, c) =>
        {
          val n = if(key > e)  Branch(l, r.insert(key), e, c)
                  else         Branch(l.insert(key), r, e, c)  
         
          flipColor(rightRotation(leftRotation(n)))
        }
    }
    
    def delete[B >: A <% Ordered[B]](key: B): Tree[A] = this match
    {
      // Search Stage
      case Leaf => Leaf
      case Branch(l, r, e, _) if(key > e) => Branch(l, r.delete(key), e, BLACK) 
      case Branch(l, r, e, _) if(key < e) => Branch(l.delete(key), r, e, BLACK)

      // Deletion Stage 
      case Branch(Leaf, r, e, _) => r
      case Branch(l, Leaf, e, _) => l
      case Branch(l, r, e, _) => 
        {
          val pred = l.max
          Branch(l.delete(pred), r, pred, BLACK)
        }
    }
    // ----- API for using Binary Search Tree as Dynamic Ordered Array -----
    private def select(order: Int): A = this match
    {
      case Leaf => throw new IndexOutOfBoundsException()
      case Branch(l, r, e, _) => 
        {
          if      (l.size + 1 == order)  e
          else if (order <= l.size)      l.select(order)
          else                           r.select(order - l.size - 1)
        }
    }
    
    def apply(idx: Int) = select(idx + 1)
    def get(idx: Int)   = select(idx + 1)
    
    def search[B >: A <% Ordered[B]](key: B): Int = this.binarySearch(key, 0, false)
    def rank  [B >: A <% Ordered[B]](key: B): Int = this.binarySearch(key, 0, true)
    private def binarySearch[B >: A <% Ordered[B]](key: B, order: Int, rank: Boolean): Int = this match
    {
      case Leaf => if(rank) order else -1
      case Branch(l, r, e, _) => 
        {
          if      (e == key)  order + l.size
          else if (key > e)   r.binarySearch(key, order + l.size + 1, rank)
          else                l.binarySearch(key, order, rank)
        }
    }
            
  }
  
  case class Branch[+A <% Ordered[A]]
        (left: Tree[A], right: Tree[A], elem: A, color: Boolean) extends Tree[A]
  { val size = left.size + right.size + 1 }
  
  object Leaf extends Tree[Nothing]
  {
     def empty  = throw new NoSuchElementException("Leaf Node")
     def left   = empty
     def right  = empty
     def elem   = empty
     
     val color  = false
     val size   = 0
     
  }  
  
  def main(args: Array[String]) = 
  {
    def insertAll(l: List[Int], root: Tree[Int]): Tree[Int] = l match
    {
      case Nil     => root
      case x :: xs => insertAll(xs, root.insert(x))
    }
    
    val list = (1 to 10000) map (x => scala.util.Random.nextInt(10000))    
    val root = insertAll(list.toList, Branch(Leaf, Leaf, -1, false))
    
    println(root.elem)
    println(root.toList)
    println(root(3))
    
  }
  
} 