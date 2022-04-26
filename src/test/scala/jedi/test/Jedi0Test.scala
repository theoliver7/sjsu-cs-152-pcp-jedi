package jedi.test

import jedi.context.alu
import jedi.expression.Identifier
import jedi.value._

object Jedi0Test extends App {

  try {
    println(Exact(10) + Exact(3))       // 13
    println(Exact(5) + Inexact(9))    // 14.0
    println(Exact(5) * Inexact(9))    // 45.0
    println(Exact(5) - Inexact(9))    // -4.0
    println(Exact(5) / Exact(9))    // 0
    println(Exact(5) / Inexact(9))    // 0.5555555555555556
    println(-Exact(5))                  // -5
    println(Exact(5) < Inexact(9))    // true
    println(Exact(5) == Exact(5))       // true
    println(Exact(5) == Inexact(5))     // true

    println(Chars("abc") + Exact(42))      // abc42
    println(Chars("abc") + Inexact(42))    // abc42.0
    println(Chars("abc") + Chars("def"))   // abcdef
    println(Chars("abc") < Chars("def"))   // true
    println(Chars("abc") == Chars("abc"))  // true
    println(Chars("abcde").subChars(Exact(1), Exact(4))) // bcd
    println(Chars("abcde").size)           // 5

    println(Boole(true) && Boole(false) || !Boole.FALSE) // true
    println(Boole.TRUE || Boole.FALSE ) // true

//     println(Exact(4) + Chars("abc"))      // context.TypeException: Numeric operand required
//     println(Inexact(9) / Inexact(0.0))    // context.IllegalValueException: Divide by 0!

    println(alu.execute(Identifier("add"), List(Exact(5), Exact(6), Exact(7))))     // 18
    println(alu.execute(Identifier("add"), List(Exact(5), Exact(6), Inexact(7))))     // 18.0
    println(alu.execute(Identifier("add"), List(Chars("abc"), Exact(6), Exact(7)))) // abc67
    println(alu.execute(Identifier("less"), List(Chars("abc"), Chars("def"))))  // true
    println(alu.execute(Identifier("equals"), List(Chars("abc"), Chars("abc"))))  // true
    println(alu.execute(Identifier("mul"), List(Exact(5), Exact(6), Exact(7))))     // 210
    println(alu.execute(Identifier("mul"), List(Exact(5), Exact(6), Inexact(7))))     // 210.0
    println(alu.execute(Identifier("sub"), List(Exact(5), Exact(7))))     // -2
    println(alu.execute(Identifier("div"), List(Exact(5), Inexact(7))))    // 0.7142857142857143
  } catch {
    case e: Exception => println(e)
  }

}
