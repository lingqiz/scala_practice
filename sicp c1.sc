object chapter1
{
	def cubeRoot(x: Double) =
	{
		def abs(a: Double) = if(a < 0) -a else a
		
		def isGoodEnough(y: Double) = abs(y * y * y - x) / abs(x) < 0.0001
		
		def iter(y: Double): Double =
		{
			if(isGoodEnough(y)) y
			else iter(improve(y))
		}
		
		def improve(y: Double): Double = ((x / (y * y)) + 2 * y) / 3
		
		iter(1)
		
	}                                         //> cubeRoot: (x: Double)Double
	
	cubeRoot(27)                              //> res0: Double = 3.0000005410641766

	def fibonacci(n: Int) =
	{
		def fibIter(a: Int, b: Int, i: Int): Int =
			if     (i == 0) a
			else   fibIter(b, a + b, i - 1)
			
		fibIter(0, 1, n)
	}                                         //> fibonacci: (n: Int)Int
	
	/*
		Divide the problem into two subproblems (smaller)
			- use the first kind of money, count number of (amount - denomination)
			- do not use the first kind of money
		Degenerate case :
			- amount == 0 -> 1
			- amount <  0 -> 0
			- avialable denomination == 0 -> 0
				
	*/
	
	def countCharge(amount: Int) =
	{
		def countIter(amount: Int, i: Int): Int =
		{
			if     (amount == 0) 1
			else if(amount <  0) 0
			else if(i > 5)       0
			// Boundary case
			else countIter(amount, i + 1) + countIter(amount - denom(i), i)
			// Divide into smaller subproblems
		}
		
		
		def denom(i: Int) = i match
		{
			case 1 => 50
			case 2 => 25
			case 3 => 10
			case 4 => 5
			case 5 => 1
		}
	
		if(amount <= 0) 0
		else countIter(amount, 1)
	}                                         //> countCharge: (amount: Int)Int
	
	
	countCharge(100)                          //> res1: Int = 292
	
	//Fast exponential mathod
	def exp(n: Int, p: Int): Int =
	{
		def isEven(x: Int) = x % 2 == 0
		def square(x: Int) = x * x
		def expIter(n: Int, p: Int): Int =
		{
			if      (p == 0)     1
			else if (isEven(p))  square(expIter(n, p / 2))
			else 							   n * expIter(n, p - 1)
		}
		
		expIter(n, p)
	}                                         //> exp: (n: Int, p: Int)Int
	
	def isPrime(n: Int) =
	{
		def iter(c: Int): Int =
		{
			if      (c * c > n)  n
			else if (n % c == 0) c
			else 	  iter(c + 1)
		}
	
		iter(2) == n
	}                                         //> isPrime: (n: Int)Boolean
	
	isPrime(17)                               //> res2: Boolean = true
	// proportional to sqrt(n)
	
	/*
		Fermat's Little Theorem
			If n is a prime number and a is any positive integer less than n
			then a raised to the n-th power % n == a
	
	*/
	
	def probIsPrime(n: Int) =
	{
		def expMod(a: Int) =
		{
			println(a)
			exp(a, n) % n == a
		}
		
		def rand = new java.util.Random().nextInt(n - 1) + 1
		def test(t: Int): Boolean =
		{
			if			(t == 0) 			 true
			else if (expMod(rand)) test(t - 1)
			else false
		}
		
		test(5)
	}                                         //> probIsPrime: (n: Int)Boolean
	
	
	
	

}