package jedi.value

import jedi.expression.Literal
import jedi.value.Value

trait Addable extends Literal {
  def +(other: Value) : Addable
}

