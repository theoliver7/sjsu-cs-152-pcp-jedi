package jedi.context

import scala.util.parsing.combinator._
import jedi.expression._
import jedi.value._

class Jedi3Parsers extends Jedi2Parsers {

  // assignment ::= identifier ~ ":=" ~ expression
  def assignment: Parser[Assignment] =
    identifier ~ (":=" ~> expression) ^^ {
      case id ~ exp => Assignment(id, exp)
    }

  // iteration ::= "while" ~ "(" ~ expression ~ ")" ~ expression
  def iteration: Parser[Iteration] =
    "while" ~ "(" ~ expression ~ ")" ~ expression ^^ {
      case "while" ~ "(" ~ condition ~ ")" ~ body => Iteration(condition, body)
    }

  //loop ::= "loop"~ "[" ~ expression ~ "]" ~ expression
  def loop: Parser[Loop] =
    "loop" ~ "[" ~ expression ~ "]" ~ expression ^^ {
      case "loop" ~ "[" ~ count ~ "]" ~ body => Loop(count, body)
    }

  // dereference ::= "[" ~ expression ~ "]"
  def dereference: Parser[FunCall] =
    "[" ~ expression ~ "]" ^^ {
      case "[" ~ dereference ~ "]" => FunCall(Identifier("dereference"), List(dereference))
    }

  override def expression: Parser[Expression] = declaration | conditional | iteration | disjunction | failure("Invalid expression")

  override def term: Parser[Expression] = loop | lambda | funCall | block | assignment | dereference | literal | "(" ~> expression <~ ")"

}