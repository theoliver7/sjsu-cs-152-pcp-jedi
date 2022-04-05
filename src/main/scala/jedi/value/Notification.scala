package jedi.value

class Notification(val msg: String) extends Value {
  override def toString: String = msg
}

object Notification{
  def apply(s: String) = new Notification(s)
  val OK: Notification = Notification("OK")
  val DONE: Notification = Notification("DONE")
  val UNSPECIFIED: Notification = Notification("UNSPECIFIED")
}

