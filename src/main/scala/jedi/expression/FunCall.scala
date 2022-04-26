package jedi.expression

import jedi.context.{Environment, alu}
import jedi.value.Value

class FunCall(val operation: Identifier, val operands: List[Expression]) extends Expression :
  override def execute(env: Environment): Value =
    val values = operands.map(_.execute(env))
    alu.execute(operation, values)

