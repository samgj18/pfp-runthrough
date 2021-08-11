package state

import cats.Functor
import cats.effect.Sync
import cats.effect.kernel.Ref
import cats.implicits._

// Let's say we have an in-memory counter

object EncapsulatingState {
  // One of the best approaches to managing state is to encapsulate state in the
  // interpreter and only expose an abstract interface with the functionality the
  // user needs.

  /**
    * OUR INTERFACE SHOULD KNOW NOTHING ABOUT STATE
    */

}

// Let’s say we need an in-memory counter that needs to be accessed and
// modified by other components. Here is what our interface could look like.

trait Counter[F[_]] {
  def incr: F[Unit]
  def get: F[Int]
}
/*
  Here F[_] is a Higher-Kinded type which represents an abstract effect,
  most of the time this effect ends up being IO. But it could be any effect
  that fits the shape.
 */

// The next step is to create an interpreter (usually this goes in another package
// and with create them with a make method. See https://github.com/samgj18/cards-game/blob/bcdd34792f1572aa63d66de47ad43aa3092d2735/src/main/scala/repositories/interpreters/InMemoryMessageRepository.scala#L13
object Counter {
  def make[F[_]: Functor: Ref.Make]: F[Counter[F]] = {
    Ref.of[F, Int](0).map { ref =>
      new Counter[F] {
        override def incr: F[Unit] = ref.update(_ + 1)

        override def get: F[Int] = ref.get
      }
    }
  }
}

// Is impossible to access the Ref outside the scope of the make function, in that way
// we ensure the "state shall not leak" principle. Remember, creation of Ref is effectful,
// thus, it allocates a mutable reference and the constructor returns F[Counter[F]].

/**
  * Remember that a new Counter will be created on every flatMap call
  */

// The Type Class F[_]: Functor: Ref.Make could be subsumed by a single Sync however always
// apply the least power principle. We also avoid Foreign Function Interface FFIs.

/** The above implementation uses an anonymous class, we can do it, however, with an interpreter
  * as a class */

object LiveCounter {
  def make[F[_]: Sync]: F[Counter[F]] =
    Ref.of[F, Int](0).map(new LiveCounter[F](_))
}
class LiveCounter[F[_]] private (
    ref: Ref[F, Int]
) extends Counter[F] {
  def incr: F[Unit] = ref.update(_ + 1)
  def get: F[Int] = ref.get
}
