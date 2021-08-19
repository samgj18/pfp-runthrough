package interpreters

import algebras.Algebras.Counter
import cats.Functor
import io.estatico.newtype.macros.newtype

/**
  * Remember: Monad, applicative functor, and functor are just functional programming patterns you can use to deal with effects like lists/arrays, trees, hashes/dictionaries, and even functions.
  * Read: https://medium.com/@lettier/your-easy-guide-to-monads-applicatives-functors-862048d61610
  */

/***
  * We would normally use two interpreters: One for testing and other for Live.
  */
object Interpreters {
  // A default interpreter using Redis.
  /*object Counter {
    @newtype case class RedisKey(value: String)

    def make[F[_]: Functor](
        key: RedisKey,
        cmd: RedisCommands[F, String, Int]
    ): Counter[F] =
      new Counter[F] {
        def incr: F[Unit] =
          cmd.incr(key.value).void

        def get: F[Int] =
          cmd.get(key.value).map(_.getOrElse(0))
      }
  }*/

  // And a test interpreter using an in-memory data structure.
  /*def testCounter[F[_]](
      ref: Ref[F, Int]
  ): Counter[F] =
    new Counter[F] {
      def incr: F[Unit] = ref.update(_ + 1)
      def get: F[Int] = ref.get
    }*/
  // As seen this helps encapsulate state and allow separation of concerns. The interface (algebra) knows nothing about the implementation details.

}
