package jedi.value

import jedi.context.TypeException
import jedi.expression.Literal

case class Boole(value: Boolean) extends Literal :
  
  def &&(other: Value): Boole =
    other match {
      case x: Boole => Boole(value && x.value)
      case _ => throw new TypeException("Boole type required")
    }

  def ||(other: Value): Boole =
    other match {
      case x: Boole => Boole(value || x.value)
      case _ => throw new TypeException("Boole type required")
    }

  def unary_! = Boole(!value)

  override def toString: String = value.toString

object Boole {
  def TRUE: Boole = Boole(true)
  def FALSE: Boole = Boole(false)
}