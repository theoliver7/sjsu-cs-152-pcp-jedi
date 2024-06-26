package jedi.context

import jedi.expression.*
import jedi.value.{Notification, *}

import scala.annotation.tailrec

object alu:

  def execute(opcode: Identifier, args: List[Value]): Value =
    opcode.name match {
      case "add" => add(args) // n-ary
      case "mul" => mul(args) // n-ary
      case "sub" => sub(args) // n-ary
      case "div" => div(args) // n-ary
      case "less" => less(args) // binary
      case "equals" => same(args) // binary
      case "more" => more(args) // binary
      case "unequals" => unequals(args) // binary
      case "not" => not(args) // unary
      case "write" => write(args)
      // TBC
      // variables
      case "dereference" => dereference(args)
      case "var" => makeVar(args)
    }


  // returns the content of args(0)
  private def dereference(args: List[Value]) =
    if args.size != 1 then throw TypeException("number of args mismatch")
    if !args.head.isInstanceOf[Variable] then throw TypeException("only type Variable can be dereferenced")
    args.head.asInstanceOf[Variable].content

  // creates a  variable cobtaining args(0)
  private def makeVar(args: List[Value]) =
    if args.size != 1 then throw TypeException("number of args mismatch")
    Variable(args.head)

  private def add(args: List[Value]): Value =
    @tailrec
    def helper(result: Addable, unseen: List[Value]): Addable =
      if (unseen.isEmpty) result
      else unseen.head match
        case h: Addable => helper(result + h, unseen.tail)
        case _ => throw TypeException("Inputs to + must be addable")

    if (args.size < 2) throw TypeException("2 or more inputs required by +")
    args.head match {
      case n: Addable => helper(n, args.tail)
      case _ => throw TypeException("Inputs to + must be addable")
    }

  private def sub(args: List[Value]): Value =
    @tailrec
    def helper(result: Numeric, unseen: List[Value]): Numeric =
      if (unseen == Nil) result
      else helper(result - unseen.head, unseen.tail)

    if (args.size < 2) throw TypeException("2 or more inputs required by -")
    args.head match {
      case n: Numeric => helper(args.head.asInstanceOf[Numeric], args.tail)
      case _ => throw TypeException("Inputs to - must be numeric")
    }

  private def mul(args: List[Value]): Value =
    def helper(result: Numeric, unseen: List[Value]): Numeric =
      if (unseen.isEmpty) result
      else unseen.head match {
        case h: Numeric => helper(result * h, unseen.tail)
        case _ => throw TypeException("Inputs to * must be addable")
      }

    if (args.size < 2) throw TypeException("2 or more inputs required by *")
    args.head match {
      case n: Numeric => helper(n, args.tail)
      case _ => throw TypeException("Inputs to * must be Numeric")
    }

  private def div(args: List[Value]): Value =
    @tailrec
    def helper(result: Numeric, unseen: List[Value]): Numeric =
      if (unseen == Nil) result
      else helper(result / unseen.head, unseen.tail)

    if (args.size < 2) throw TypeException("2 or more inputs required for /")
    args.head match {
      case n: Numeric => helper(args.head.asInstanceOf[Numeric], args.tail)
      case _ => throw TypeException("Inputs need to be numeric for /")
    }


  private def less(args: List[Value]): Value =
    if (args.size != 2) throw TypeException("2 inputs required by <")
    args.head match
      case x: Ordered[Value] => Boole(x < args(1))
      case _ => throw TypeException("Inputs to < must be orderable")

  private def write(args: List[Value]): Value =
    println(args.head)
    Notification.DONE

  private def same(args: List[Value]): Value = {
    if (args.size != 2) throw TypeException("2 inputs required by ==")
    Boole(args.head == args(1))
  }

  private def more(args: List[Value]): Value = {
    if (args.size != 2) throw TypeException("2 inputs required by <")
    Boole(args.head.asInstanceOf[Ordered[Value]] > args(1))
  }

  private def unequals(args: List[Value]): Value = {
    if (args.size != 2) throw TypeException("2 inputs required by !=")
    Boole(args.head != args(1))
  }

  private def not(args: List[Value]): Value = {
    if (args.size != 1) throw TypeException("1 input required by !")
    if (!args.head.isInstanceOf[Boole]) throw TypeException("Inputs to ! must be boolean")
    !args.head.asInstanceOf[Boole]
  }


// etc.
