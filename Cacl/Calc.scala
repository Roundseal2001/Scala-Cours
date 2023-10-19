abstract class Tree

case class Sum(l:Tree, r:Tree) extends Tree

case class Var(n:String) extends Tree

case class Const(v:Int) extends Tree 


object TestTree {
	type Environement = String => Int

	def eval(t: Tree, env: Environement): Int = {
		t match {
			case Sum(a,b) => eval(a,env) + eval(b,env)
			case Var(c) => env(c)
			case Const(d) => d
		}
	}

	def derive(t: Tree, v: String): Tree = {
		t match {
			case Sum(l,r) => Sum(derive(l,v),derive(r,v))
			case Var(n) => if(n == v){Const(1)}else{Const(0)}
			case Const(v) => Const(0)
		}
	}

	def main(args: Array[String]):Unit = {
		val env: Environement = {case "x" => 5 case "y" => 7}
		val exp:Tree = Sum(Sum(Var("x"),Var("x")), Sum(Const(7),Var("y")))

		println("Expression: "+ exp)
		println("Valeur: "+eval(exp,env))
		println("Dérivée: "+ derive(exp, "x"))
	}
}