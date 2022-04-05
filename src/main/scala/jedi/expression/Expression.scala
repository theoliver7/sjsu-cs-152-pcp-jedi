package jedi.expression

import jedi.context.Environment
import jedi.value.Value
trait Expression:
  def execute(env: Environment): Value
