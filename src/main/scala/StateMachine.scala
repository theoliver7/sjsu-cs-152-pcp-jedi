
class StateMachine[S](var currentState: Int, var finalState: Int):
  def execute(instructions: List[Int => Int]): Unit =
    for (x <- instructions) {
      currentState = x(currentState)
      if (currentState == finalState) return
    }


object StateMachineTest extends App {
  val mac = new StateMachine[Int](3, 10)
  // computes f(s) = ((s * s) + 1) / 0
  mac.execute(
    List(
      (s: Int) => s * s,
      (s: Int) => s + 1,
      (s: Int) => s / 0)) // never reached
  println(mac.currentState) // prints 10

  mac.currentState = 5
  mac.finalState = 100
  // computes g(s) = 2^s / 2 - 3
  mac.execute(
    List(
      (s: Int) => math.pow(2, s).toInt,
      (s: Int) => s / 2,
      (s: Int) => s - 3
    )
  )
  println(mac.currentState) // prints 13
}