package jedi.expression

import jedi.context.{Environment, TypeException}
import jedi.value.{Notification, Value, Variable}

case class Assignment(vbl: Identifier, update: Expression) extends SpecialForm :
  override def execute(env: Environment): Value =
    if !env(vbl).isInstanceOf[Variable] then throw new TypeException("vbl is not of type Variable")
    env(vbl).asInstanceOf[Variable].content = update.execute(env)
    Notification.DONE