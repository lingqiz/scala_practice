package review.scala

object PatternMathcing 
{
  // Pattern is useful to deal with algebraic data type 
  // Decomposition based on the "shape" of objects 
  
  def insertionSort(xs: List[Int]): List[Int] = 
  {
    def insert(x: Int, sorted: List[Int]): List[Int] = sorted match 
    {
      case Nil      => x :: Nil
      case a :: as  => if(x <= a) x :: a :: as else a :: insert(x, as)
    }
    
    xs match 
    {
      case Nil     => Nil
      case y :: ys => insert(y, insertionSort(ys)) 
    }
  }
  
  // Merge Sort using pattern matching 
  def mergeSort(xs: List[Int]): List[Int] = 
  {
    def merge(ils: List[Int], irs: List[Int]): List[Int] = 
      (ils, irs) match
      {
        case (Nil, rs) => rs
        case (ls, Nil) => ls
        case (l :: ls, r :: rs) => 
          {
            if(l <= r) l :: merge(ls, irs)
            else       r :: merge(ils, rs)
          }
      }
    
    val n = xs.length / 2
    
    if(n == 0) xs
    else
    {
      val (lhl, rhl) = xs splitAt n
      merge(mergeSort(lhl), mergeSort(rhl))
    }
  }
   
    def quickSort(rxs: List[Int]): List[Int] =  
    {         
      
      def partition(in: List[Int], pivot: Int) =
      {
          def recPrt(ps: List[Int], lps: List[Int], rps: List[Int]): (List[Int], List[Int]) = ps match 
          {
            case Nil      => (lps, rps)
            case p :: pss => if(p < pivot) recPrt(pss, p :: lps, rps) 
                             else          recPrt(pss, lps, p :: rps)
          }
          
          recPrt(in, Nil, Nil)
      }
      
      rxs match
      {
        case Nil          => rxs
        case head :: Nil  => rxs
        case head :: tail => 
          {
            val (left, right) = partition(tail, head)
            quickSort(left) ::: head :: quickSort(right)
          }          
      }      
    }    
    
    def isSorted(xs: List[Int]): Boolean = xs match
    {
      case Nil              => true
      case head :: Nil      => true
      case fi :: sec :: xss => if(fi > sec) false else isSorted(sec :: xss)
    }
  
  def main(args: Array[String])
  {
    val list = 1 to 300000 map (x => new java.util.Random().nextInt(3000000 * 10))
    val sl   = list.toList
    
    val start = System.currentTimeMillis();
    println(isSorted(quickSort(sl)))
    println(System.currentTimeMillis() - start);
    
  }
}