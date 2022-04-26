package jedi.expression

import jedi.context.Environment
import jedi.value.{Notification, Value}

case class Declaration(identifier: Identifier, expression: Expression) extends SpecialForm :
  override def execute(env: Environment): Value =
    env.put(identifier, expression.execute(env))
    Notification.OK

