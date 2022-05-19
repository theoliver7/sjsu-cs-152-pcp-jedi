package jedi.expression

import jedi.context.Environment
import jedi.value.{Boole, Value, Notification}
import jedi.context.{JediException, TypeException}

class Iteration(condition: Expression, body: Expression) extends SpecialForm {
  override def execute(env: Environment): Value =
    if (!condition.execute(env).isInstanceOf[Boole]) then throw TypeException("condition must be Boole")
    while (condition.execute(env).asInstanceOf[Boole].value) {
      body.execute(env)
    }
    Notification.DONE

}
