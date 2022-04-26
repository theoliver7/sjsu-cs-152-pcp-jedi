package jedi.value

import jedi.context.*

case class Inexact(value: Double) extends Numeric with Ordered[Value] :

  override def +(other: Value): Addable =
    other match
      case x: Exact => Inexact(this.value + x.value)
      case x: Inexact => Inexact(this.value + x.value)
      case _ => throw new TypeException("Numeric operand required")

  override def /(other: Value): Numeric =
    other match
      case x: Exact =>
        if (x.value == 0) throw IllegalValueException("Can't divide by 0")
        else Inexact(this.value.toInt / x.value)
      case x: Inexact =>
        if (x.value == 0.0) throw IllegalValueException("Can't divide by 0")
        else Inexact(this.value / x.value)
      case _ => throw new TypeException("Numeric operand required")

  override def *(other: Value): Numeric =
    other match
      case x: Exact => Inexact(this.value.toInt * x.value)
      case x: Inexact => Inexact(this.value * x.value)
      case _ => throw new TypeException("Numeric operand required")

  override def -(other: Value): Numeric =
    other match
      case x: Exact => Inexact(this.value.toInt - x.value.toInt)
      case x: Inexact => Inexact(this.value - x.value)
      case _ => throw new TypeException("Numeric operand required")


  override def unary_- = Inexact(-value)

  override def compare(that: Value): Int =
    that match {
      case x: Exact => this.value.toDouble.compare(x.value)
      case x: Inexact => this.value.compare(x.value)
      case _ => throw new TypeException("Arguments must be comparable")
    }

  override def equals(other: Any): Boolean =
    other match {
      case x: Inexact => x.isInstanceOf[Inexact] && x.value == this.value.toDouble
      case x: Exact => x.isInstanceOf[Exact] && x.value == this.value
      case _ => false
    }
    
  override def toString: String = value.toString

  override def hashCode: Int = this.toString.hashCode
