sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head:A,tail:List[A]) extends List[A] 

object List {
	def apply[A](as:A*):List[A] =
		if(as.isEmpty) Nil
		else Cons(as.head, apply(as.tail: _*))


	def tail[A](l:List[A]): List[A] = l match {
		case Nil => Nil
		case Cons(_,t) => t
	}

	def drop[A](l:List[A], n:Int):List[A] = {

		//1er Exemple
		if(n>0) l match {
			case Nil => Nil
			case Cons(_,t) => drop(t, n-1)
		}else{
			l
		}

		//2ème Exemple
		l match {
			case Cons(_,t) if (n>0) => drop(t,n-1)
			case _ => l
		}

		//3ème Exemple
		if(n>0) drop(tail(l), n-1) else l}

	def dropWhile[A](l:List[A])(f:A=> Boolean): List[A] = {
		l match {
			case Cons(h,t) if (f(h)) => dropWhile(t)(f)
			case _ => l
		}
	}

	def setHead[A](l:List[A],newHead: A):List[A]= {
		l match{
			case Nil => sys.error("Liste vide")
			case Cons(_,tail) => Cons(newHead,tail)
		}
	}

	def append[A](a1:List[A],a2:List[A]):List[A] = a1 match{
		case Nil => a2
		case Cons(h,t) => Cons(h, append(t,a2))
	}

	def init[A](l: List[A]):List[A] = l match {
		case Nil => Nil
		case Cons(_,Nil) => Nil
		case Cons(h,t) => Cons(h,init(t))
	}

	def foldRight[A,B](l:List[A],z:B)(f:(A,B) => B):B = {
		l match {
			case Nil => z
			case Cons(x,xs) => f(x,foldRight(xs,z)(f))
		}
	}

	def foldLeft[A,B](l:List[A],z:B)(f:(B,A) => B):B = {
		l match {
			case Nil => z
			case Cons(a,b) => foldLeft(b,f(z,a))(f)
		}
	}

	def sum(ints:List[Int]):Int = ints match {
		case Nil => 0
		case Cons(h,t) => h + sum(t)
	}

	def product(ds:List[Double]):Double = ds match {
		case Nil => 1.0
		case Cons(0.0,_) => 0.0
		case Cons(h,t) => h * product(t)
	}

	def sum2(l:List[Int])= foldRight(l,0)(_+_)

	def product2(l:List[Double]) = foldRight(l,1.0)(_*_)

	def length[A](l:List[A]):Int = foldRight(l, 0)((_, acc) => acc + 1)

	def sum3(l:List[Int])= foldLeft(l,0)(_+_)

	def product3(l:List[Double]) = foldLeft(l,1.0)(_*_)

	def length2[A](l:List[A]):Int = foldLeft(l, 0)((acc,_) => acc + 1)

	def reverse[A](l:List[A]):List[A] = foldLeft(l,Nil:List[A])((acc,x) => Cons(x,acc))

	// Difficile
	def foldRightToLeft[A,B](l:List[A],z:B)(f:(A,B) => B):B = {
		foldLeft(reverse(l),z)((b,a) => f(a,b))
	}

	def foldLeftToRight[A,B](l:List[A],z:B)(f:(B,A) => B):B = {
		foldRight(reverse(l),z)((a,b) => f(b,a))
	}

	def appendRight[A](l1:List[A],l2:List[A]):List[A]= foldRight(l1,l2)(Cons(_,_))

	def appendLeft[A](l1:List[A],l2:List[A]):List[A]= foldLeft(reverse(l1),l2)((a,b) => Cons(b,a))

	def flatten[A](l:List[List[A]]):List[A]= foldRight(l,List[A]())(append)

	def addOne(l:List[Int]):List[Int]= foldRight(l,List[Int]())((h,t) => Cons((h+ 1),t))

	def doubleToString(l:List[Double]):List[String]= foldRight(l,List[String]())((h,t)=> Cons(h.toString,t))

	def map[A,B](l:List[A])(f:A => B):List[B] = foldRight(l, List[B]())((h,t)=> Cons(f(h),t))
	
	def filter[A](l: List[A])(pred: (A) => Boolean):List[A] = foldRight(l,List[A]())((h,t) => if(pred(h)) Cons(h,t) else t)

	def flatMap[A,B](l:List[A])(f:A => List[B]):List[B] = flatten(map(l)(f))


}  

object TestList {
	import List._

	def main(args: Array[String]): Unit = {
		val l = List(1,2,3)
		val l2 = List(1.0,2.0,3.0)
		val l1 = Cons(1, Cons(2 , Cons(3, Nil)))
		val l3 = List(List(1,2), List(3,4), List(5))
		println(flatMap(l)(i => Cons(i,i)))
	}
}

// val x = List(1,2,3,4,5) match {
// 	case Cons(x,Cons(2, Cons(4, _))) => x
// 	case Nil=> 42
// 	case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x+y
// 	case Cons(h,t) => h + sum(t)
// 	case _ => 101
// }

