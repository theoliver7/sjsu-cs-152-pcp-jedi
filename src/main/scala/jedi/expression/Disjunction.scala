package jedi.expression

import jedi.context.{Environment, TypeException}
import jedi.value.{Boole, Value}

case class Disjunction(expressions: List[Expression]) extends SpecialForm :

  override def execute(env: Environment): Value =
    if (expressions.length == 1) then expressions.head.execute(env).asInstanceOf[Boole]
    else {
      val headValue = expressions.head.execute(env).asInstanceOf[Boole]
      if (!headValue.value)
        Disjunction(expressions.tail).execute(env)
      else headValue
    }
