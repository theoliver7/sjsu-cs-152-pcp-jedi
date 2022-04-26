package jedi.expression

import jedi.context.Environment
import jedi.value.Value

case class Identifier(name: String) extends Expression {

  override def execute(env: Environment): Value =
    env.apply(this)

  override def toString: String = name

  override def hashCode(): Int = toString.hashCode
}