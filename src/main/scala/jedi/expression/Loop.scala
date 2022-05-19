package jedi.expression

import jedi.context.Environment
import jedi.context.TypeException
import jedi.value.{Exact, Notification, Value}

class Loop(count: Expression, body: Expression) extends SpecialForm :
  override def execute(env: Environment): Value =
    var counter = 0
    if (!count.execute(env).isInstanceOf[Exact]) then throw TypeException("count must be of type Exact")

    while (counter < count.execute(env).asInstanceOf[Exact].value) {
      body.execute(env)
      counter = counter + 1
    }
    Notification.DONE

