package errorhandling

import cats.{ApplicativeThrow, Functor, MonadThrow}
import cats.effect.std.Random
import cats.implicits._
import errorhandling.ErrorHandling.BusinessError.RandomError

import scala.util.control.NoStackTrace

object ErrorHandling {
  // This section is completely biased as of now, but it has worked for a wide variety of engineers over the years.

  /**
    * There are two types of errors: Monad Error and Applicative Error.
    *
    * We normally work in the context of some parametric effect F[_]. When using Cats Effect, we can rely on Monad Error / Applicative Error
    * and its functions (attempt, handleErrorWith, rethrow) to deal with errors since the IO monad implements MonadError[F, Throwable], also aliased MonadThrow.
    */

  // Example: Say we have a Categories algebra that lets us find all the available categories.
  case class Category()

  trait Categories[F[_]] {
    def findAll: F[List[Category]]
    def maybeFindAll: F[Either[RandomError, List[Category]]]
  }

  sealed trait BusinessError extends NoStackTrace
  object BusinessError {
    type RandomError = RandomError.type
    case object RandomError extends BusinessError
  }

  object Categories {
    def make[F[_]: MonadThrow: Random]: Categories[F] =
      new Categories[F] {
        def findAll: F[List[Category]] =
          Random[F].nextInt.flatMap {
            case n if n > 100 => List.empty[Category].pure[F]
            case _            => RandomError.raiseError[F, List[Category]]
          }

        def maybeFindAll: F[Either[RandomError, List[Category]]] = {
          Random[F].nextInt.map {
            case n if n > 100 => List.empty[Category].asRight[RandomError]
            case _            => RandomError.asLeft[List[Category]]
          }
        }
      }
  }

  // The question arises: Would it be better to be specific about the error type and change its signature to something like -> def maybeFindAll: F[Either[RandomError, List[Category]]]

  /**
    * It depends: My recommendation is to only do it when it is really necessary. At all times, the question you need to ask yourself is What am I going to do with the error information?
    * Most of the time you only need to deal with the successful case and let it fail in case of error. However, there are valid cases for explicit error handling.
    * For a web application, one of my favorites is to handle business errors at the HTTP layer to return different HTTP response codes, in which case we don’t really need to use Either.
    */

  // With the Signature def maybeFindAll: F[Either[RandomError, List[Category]]] and we need to handle different cases for either a MonadError or List[Category]
  class Program[F[_]: Functor](
      categories: Categories[F]
  ) {
    def findAll: F[List[Category]] =
      categories.maybeFindAll.map {
        case Right(c)          => c
        case Left(RandomError) => List.empty[Category]
      }

  }

  // Now, we can achieve the exact same without having the error signature on the interface:
  class SameProgram[F[_]: ApplicativeThrow](
      categories: Categories[F]
  ) {
    def findAll: F[List[Category]] = {
      // Total version to unknown throwable (In case we add more errors to BusinessError ADT) -> categories.findAll.handleError(throwable => List.empty[Category])
      categories.findAll.recover {
        case RandomError => List.empty[Category]
      }
    }
  }

  // Summary:

  /**
    * We have three ways of handling errors:
    * 1. MonadError, ApplicativeError and EitherMonad
    */
}
