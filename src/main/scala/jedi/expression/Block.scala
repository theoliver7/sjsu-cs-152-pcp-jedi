package jedi.expression
import jedi.context.Environment
import jedi.value.Value

class Block(expressions: List[Expression]) extends SpecialForm:

  override def execute(env: Environment): Value =
    val tempEnv = new Environment(env)
    expressions.map(_.execute(tempEnv)).last

