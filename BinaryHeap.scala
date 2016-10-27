package pq
class BinaryHeap 
{
  /**
   * Complete Binary Tree where the Root Node is the Maximum 
   */
  abstract sealed class Heap[+A <% Ordered[A]]
  {
    def elem: A
    
    def left:    Heap[A]
    def right:   Heap[A]
    def isEmpty: Boolean
    
    val size:   Int
    val height: Int
    
    def fail(m: String) = throw new NoSuchElementException(m)
    
    def max: A = elem
    def delMax : (A, Heap[A]) = 
    {
      val root  = elem
      
      def floatLeft[A <% Ordered[A]](x: A, l: Heap[A], r: Heap[A]): Heap[A] = l match 
      {
        case Node(y, lt, rt) => Node(y, Node(x, lt, rt), r)
        case _ => Node(x, l, r)
      }
      
      def floatRight[A <% Ordered[A]](x: A, l: Heap[A], r: Heap[A]): Heap[A] = r match 
      {
        case Node(y, lt, rt) => Node(y, l, Node(x, lt, rt))
        case _ => Node(x, l, r)
      }
      
      def mergeChildren(l: Heap[A], r: Heap[A]): Heap[A] = 
      if (l.isEmpty && r.isEmpty) Heap.empty
      else if (l.size < math.pow(2, l.height) - 1) 
        floatLeft(l.elem, mergeChildren(l.left, l.right), r)
      else if (r.size < math.pow(2, r.height) - 1)
        floatRight(r.elem, l, mergeChildren(r.left, r.right))
      else if (r.height < l.height)
        floatLeft(l.elem, mergeChildren(l.left, l.right), r)
      else floatRight(r.elem, l, mergeChildren(r.left, r.right))
      
      val merge = mergeChildren(left, right)
      
      if(merge.isEmpty) (root, Leaf)
      else (root, Heap.sink(merge.elem, merge.left, merge.right))
      
    }
    private def sink: Heap[A] = Heap.sink(elem, left, right)
    
    
    def insert[B >: A <% Ordered[B]](x: B): Heap[B] = 
    {
      if      (isEmpty) Node(x, Leaf, Leaf)
      else if (left.size  < math.pow(2, left.height) - 1)  Heap.swim(elem, left.insert(x), right)
      else if (right.size < math.pow(2, right.height) - 1) Heap.swim(elem, left, right.insert(x))
      else if (right.height < left.height)                 Heap.swim(elem, left, right.insert(x))
      else Heap.swim(elem, left.insert(x), right)
    }
  }
  
  object Heap
  {
    def empty[A]: Heap[A] = Leaf
    
    private[Heap] def swim[A <% Ordered[A]](x: A, l: Heap[A], r: Heap[A]): Heap[A] =
      (l, r) match
    {
      case (Node(e, ll, lr), _) if(e > x) =>
        Node(e, Node(x, ll, lr), r)
        
      case (_, Node(e, rl, rr)) if(e > x) => 
        Node(e, l, Node(x, rl, rr))
        
      case(_, _) => Node(x, l, r)
    }
    
    private[Heap] def sink[A <% Ordered[A]](x: A, l: Heap[A], r: Heap[A]): Heap[A] =
      (l, r) match
    {      
      case(Node(le, ll, lr), Node(re, rl, rr)) if(re > le && re > x) =>
        Node(re, l, sink(x, rl, rr))
        
      case(Node(le, ll, lr), _) if(le > x) =>
        Node(le, sink(x, ll, lr), r)
        
      case(_, _) => Node(x, l, r)
    }
  }
  
  case object Leaf extends Heap[Nothing]
  {
    val size    = 0
    val height  = 0
    val isEmpty = true
    
    def elem  = fail("Leaf Node")
    def left  = fail("Leaf Node")
    def right = fail("Leaf Node")
  }
  
  case class Node[+A <% Ordered[A]](elem: A, left: Heap[A], right: Heap[A]) extends Heap[A]
  {
      val isEmpty = false
      val size    = left.size + right.size + 1
      val height  = math.max(left.height, right.height) + 1
  }
  
}