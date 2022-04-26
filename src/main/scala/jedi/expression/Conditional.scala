package jedi.expression

import jedi.context.Environment
import jedi.value._

case class Conditional(condition: Expression, next: Expression, alt: Expression = null) extends SpecialForm {
  override def execute(env: Environment): Value =
    if (condition.execute(env).asInstanceOf[Boole].value) then next.execute(env)
    else if (alt != null) then alt.execute(env)
    else Notification.UNSPECIFIED
}
