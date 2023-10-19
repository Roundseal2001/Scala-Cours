object MyModule {

	def abs(n:Int):Int =
		if(n<0)-n else n 

	private def formatAbs(x:Int) = {
		val msg = "The absolute value of %d is %d"
		msg.format(x, abs(x))
	}

	def main(args:Array[String]):Unit = {
		// println(formatResult("fibonacci",2,fib))
		// println(formatResult("increment",7,(x) => {x + 1})) // plusieur 
		// println(formatResult("increment",7,x=> x + 1)) // seul
		// println(formatResult("increment",7,_+1))
		// val a = Array(1,2,3,4,5)
		// println(isSorted(a,(x,y) => x < y))
		// val b = Array(1,3,2,4,5)
		// println(isSorted(b,(x,y) => x < y))
		// val f = partial1("Prefixe_",(x,y) => x+y)
		// println(f(42))
		// val c = curry((x:String, y:Int) => x+y)
		// println(c("Prefixe_"))
		// println(c("Prefixe_")(42))
	}

	def factorial(n:Int):Int={
		@annotation.tailrec
		def loop(n:Int, acc:Int):Int = if(n<0) acc else loop(n-1,n*acc)
			loop(n,1)
	}

	def fib(n:Int):Int={
		@annotation.tailrec
		def loop(n:Int,f:Int,s:Int):Int = if(n<=0) f else loop(n-1,s,f+s)
		
		loop(n,0,1)
	}

	private def formatResult(name:String,n:Int,f:Int => Int):String = {
		"The %s of %d is %d".format(name,n,f(n))
	}

	def findFirst[A](s:Array[A],p:A => Boolean):Int ={
		@annotation.tailrec
		def loop(n:Int):Int =
			if(n > s.length) - 1
			else if (p(s(n))) n
			else loop(n+1)
		loop(0)	
	}

	def isSorted[A](a:Array[A], gt:(A,A) => Boolean):Boolean ={
		@annotation.tailrec
		def loop(n:Int):Boolean =
			if(n >= a.length-1) true
			else if(!gt(a(n),a(n+1))) false
			else loop(n+1)
		loop(0)
	}

	def partial1[A,B,C](a:A, f:(A,B) => C): B => C ={
		// (b: B) => f(a,b)
		(b) => f(a,b)
		// f(a,_)
	}

	def curry[A,B,C](f:(A,B)=> C): A => (B => C) ={
		(a) => (b) => f(a,b)
	}

	def uncurry[A,B,C](f: A => B => C): (A,B) => C ={
		(a,b) => f(a)(b)
	}

	def compose[A,B,C](f:B => C, g:A =>B): A => C = {
		(a) => f(g(a)) 
	}

}