package jedi.expression

import jedi.context.Environment
import jedi.value.Value

trait Literal extends Expression with Value:
  override def execute(env: Environment): Value = this

