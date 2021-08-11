package state

import cats.data.State

object SequentialVsConcurrent {
  // When should we use Ref? It all boils down to whether we need sequential or concurrent state.

  // If program state can be sequential we can use State Monad if not any type that supports
  // concurrency.
  val nextInt: State[Int, Int] = State(s => (s + 1, s * 2))
  def seq: State[Int, Int] =
    for {
      n1 <- nextInt
      n2 <- nextInt
      n3 <- nextInt
    } yield n1 + n2 + n3

  /*
    In scenarios were  an interface might be invoked in many places we wanna use concurrency-safe implementations
    such as Ref. Ref is a purely functional model of concurrent mutable reference. It's atomic update and modify
    allows compositionality and concurrency safety.
   */

}
