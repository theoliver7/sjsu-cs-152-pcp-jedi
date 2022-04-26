package jedi.expression

import jedi.context.Environment
import jedi.value._

case class Conjunction(expressions: List[Expression]) extends SpecialForm :
  override def execute(env: Environment): Value =
    if (expressions.length == 1) then expressions.head.execute(env).asInstanceOf[Boole]
    else {
      val headValue = expressions.head.execute(env).asInstanceOf[Boole]
      if (headValue.value)
        headValue && Conjunction(expressions.tail).execute(env)
      else headValue
    }

