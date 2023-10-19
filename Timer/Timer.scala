object Timer {
	def oncePerSecond(callback:() => Unit) = {
		while(true) {callback(); Thread.sleep(1000)}
	}
	// def timesFlies()= {
	// 	println("tick")
	// }
		def main(args:Array[String])= {
		oncePerSecond(() => println("tick"))
	}

}