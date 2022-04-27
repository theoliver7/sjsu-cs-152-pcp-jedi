package jedi.value

import jedi.context.Environment
import jedi.expression.{Expression, Identifier}

class Closure(val defEnv: Environment, val params: List[Identifier], val body: Expression) extends Value :
  
  def apply(arguments: List[Value]): Value =
    val tempEnv = new Environment(defEnv)
    tempEnv.bulkPut(params, arguments)
    body.execute(tempEnv)

