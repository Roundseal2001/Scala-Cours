sealed trait Option[+A]
case object None extends Option[Nothing]
case classe Some[+A](get:A) extends Option[A]

