package jedi.expression

import jedi.context.Environment

case class Identifier(val name: String) extends Expression {
  override def execute(env: Environment) = ???
  override def toString: String = name
}