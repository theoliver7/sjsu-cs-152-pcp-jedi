package jedi.value

import jedi.context.TypeException

case class Chars(value: String) extends Addable with Ordered[Value]{
  def size(): Exact = Exact(value.length)

  def subChars(from: Exact, to: Exact): Chars = Chars(value.substring(from.value,to.value))

  override def +(other: Value): Addable =
    other match {
      case x: Chars => Chars(value + x.value)
      case x: Boole => Chars(value + x.toString)
      case x: Exact => Chars(value + x.toString)
      case x: Inexact => Chars(value + x.toString)
      case _ => throw new TypeException("String operand required")
    }

  override def compare(that: Value): Int = that match {
    case x: Chars => value.compare(x.value)
    case _ => throw new TypeException("Arguments must be comparable")
  }
}
