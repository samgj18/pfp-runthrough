package antipatterns
import antipatterns.AntiPatterns.Api.Answer
import antipatterns.AntiPatterns.Pred.{Discard, Keep}
import antipatterns.AntiPatterns.Proxy.Result
import antipatterns.AntiPatterns.Proxy.Result.{No, Yes}
import cats.Functor
import cats.implicits._
import cats.effect._
import newtypes.User

import java.util.UUID

object AntiPatterns extends App {
  // 1. Thou shalt not use Seq in your interface
  case class Item(price: Int)
  trait Items[F[_]] {
    def getAll: F[Seq[Item]]
  }
  class Program[F[_]: Sync](items: Items[F]) {

    def calcTotalPrice: F[BigDecimal] =
      items.getAll.map { seq =>
        seq.toList
          .map(_.price)
          .sum
      }
  }

  /**
    * Problem:
    *
    * How do we know it is safe to call toList? What if the Items interpreter uses a Stream (or LazyList since Scala 2.13.0)
    * representing possibly infinite items? It would still be compatible with our interface, yet, it will have different semantics.
    *
    * Solution:
    *
    * To be safer always use specific data types i.e: List, Vector, Chain, fs2.Stream
    */

  // 2. Thou shalt not use Monad Transformers in your interface

  trait Users[F[_]] {
    //def findUser(id: UUID): OptionT[F, User] <- No need for this as is problematic
    def findUser(id: UUID): F[Option[User]]
  }

  /**
    * Problem:
    *
    * Committing to a specific Monad Transformer kills compositionality for the API users.
    *
    * Solution:
    *
    * Let typeclass constraints dictate what F is capable of in your programs.
    *
    * Hint: Use NoStackTrace instead of Exception for custom error ADTs
    * case object UserNotFound extends NoStackTrace
    */

  // 3. This one is more a code-smell rather than a anti-pattern. Something to avoid whenever possible.
  abstract class CustomList[A] {
    def filter(p: A => Boolean): CustomList[A]
    def filterBy(p: A => Pred): CustomList[A] // <- Improved version
  }

  /**
    * Problem:
    *
    * In the example what does filter mean? Does it keep the results according to the predicate?
    * Or does it discard them? We can’t really tell by the type signature.
    *
    * Solution:
    */
  sealed trait Pred
  object Pred {
    case object Keep extends Pred
    case object Discard extends Pred
  }
  // How to use it?

  // 1. With our own data types
  /*
    CustomList
      .range(1, 11)
      .filterBy { n =>
        if (n > 5) Pred.Keep else Pred.Discard
      } // res0: List(6, 7, 8, 9, 10)
   */

  // 2. We can expose our custom filterBy as an extension method for any List[A].
  implicit class ListOps[A](xs: List[A]) {
    def filterBy(p: A => Pred): List[A] =
      xs.filter {
        p(_) match {
          case Keep    => true
          case Discard => false
        }
      }
  }
  List
    .range(1, 11)
    .filterBy { n =>
      if (n > 5) Keep else Discard
    }
  // res0: List(6, 7, 8, 9, 10)

  // 4. Thou shalt not use the ifM extension method (Remember always ADT over Boolean

  trait BoolApi[F[_]] {
    def get: F[Boolean]
  }
  // Usage: api.get.ifM(IO.println("YES"), IO.println("NO"))

  trait Api[F[_]] {
    def get: F[Answer]
  }
  /* New Usage:
    api.get.flatMap {
      case Answer.Yes => IO.println("YES!")
      case Answer.No  => IO.println("NO!")
  }*/

  object Api {
    sealed trait Answer
    object Answer {
      case object Yes extends Answer
      case object No extends Answer
    }
  }

  // All hail the proxy!

  // In many cases we will deal with public APIs that expose functions returning booleans. This is very common for libraries,
  // however is bad from the user perspective, always avoid boolean blindness.
  trait Proxy[F[_]] {
    def get: F[Result]
  }
  object Proxy {
    sealed trait Result
    object Result {
      case object Yes extends Result
      case object No extends Result
    }

    def make[F[_]: Functor](
        boolApi: BoolApi[F]
    ): Proxy[F] =
      new Proxy[F] {
        def get: F[Result] =
          boolApi.get.map(
            if (_) Yes else No
          )
      }
  }

  /**
    * Problem:
    *
    * Public APIs may return booleans
    *
    * Solution:
    *
    * Build proxies atop of them to ensure your internal representation is as descriptive as possible
    */

  // Summary: How far should we push this approach? I would recommend to stay away from boolean blindness in critical components
  // but to be flexible in the rest of the application. It is always a matter of agreement within your team.
}
