package jedi.expression

import jedi.context.{Environment, alu}
import jedi.value.{Closure, Value}

class FunCall(val operation: Identifier, val operands: List[Expression]) extends Expression :
  override def execute(env: Environment): Value =
    if (env.contains(operation)) {
      env(operation).asInstanceOf[Closure].apply(operands.map(_.execute(env)))
    } else {
      alu.execute(operation, operands.map(_.execute(env)))
    }

