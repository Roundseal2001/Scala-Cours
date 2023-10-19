trait Ord {
	def < (that: Any): Boolean 
	def <= (that: Any): Boolean = (this < that) || (this == that)
	def > (that: Any): Boolean = !(this <= that)
	def >= (that: Any): Boolean = !(this < that)
}

case class Date(y:Int,m:Int,d:Int) extends Ord{


	// def < (that:Any): Boolean = {
	// 	if(!that.isInstanceOf[Date])
	// 		sys.error("cannot compare "+that+" with a Date")
		
	// 	val i = that.asInstanceOf[Date]
	// 	(year < i.year) || (year == i.year && (month < i.month || (month == i.month && day < i.day)))
	// }

	def < (that:Any): Boolean = that match {
		case Date(yy,mm,dd) => (yy == y && (mm < m || (mm == m && dd < d)))
		case _ => sys.error("cannot compare "+ that + " with a Date")
	}
}
