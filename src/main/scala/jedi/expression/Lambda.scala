package jedi.expression

import jedi.context.Environment
import jedi.value.Value
import jedi.value.Closure

class Lambda(val lambda: List[Identifier], val body: Expression) extends SpecialForm :

  override def execute(env: Environment): Value =
    Closure(env, lambda, body)

