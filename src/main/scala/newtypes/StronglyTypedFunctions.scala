package newtypes

import cats.effect._
import cats.implicits._

case class Username(val value: String) extends AnyVal
case class Email(val value: String) extends AnyVal
case class PrivateUsername private (val value: String) extends AnyVal
case class PrivateEmail private (val value: String) extends AnyVal
sealed abstract class SealedPrivateUsername(
    val value: String
) // == sealed abstract case class newtypes.Username(value: String)
sealed abstract class SealedPrivateEmail(val value: String)
case class User(username: String, email: String)

trait Functions[F[_]] {
  def lookup(username: String, email: String): F[Option[User]]
}

object StronglyTypedFunctions {

  def lookup(username: String, email: String): IO[Option[User]] = ???
  // See the issue? It is not only easy to confuse the order of
  // the parameters but it is also straightforward to feed our function with invalid data!
  lookup("", "")

  /**
    * 1. Value classes
    * In vanilla Scala, we can wrap a single field and extend the AnyVal abstract class to avoid some runtime costs.
    * Here is how we can define value classes for username and email.
    * See newtypes.Username, newtypes.Email above
    */

  // Notice that we can no longer confuse the order of the parameters. Can we?
  def lookupValueClass(user: Username, email: Email): IO[Option[User]] = ???
  lookupValueClass(Username("aeinstein@research.com"), Email("aeinstein"))

  // We still can mess this up
  lookupValueClass(Username("aeinstein@research.com"), Email("aeinstein"))
  lookupValueClass(Username("aeinstein"), Email("123"))
  lookupValueClass(Username(""), Email(""))

  // Even though we're doing it on purpose the compiler should help us to avoid this type of mistakes, but how?
  // A way to communicate our intentions to the compiler is to make the case class constructors private only expose smart constructors.

  def lookupPrivateValueClass(
      user: PrivateUsername,
      email: PrivateEmail
  ): IO[Option[User]] = ???
  lookupValueClass(Username("aeinstein@research.com"), Email("aeinstein"))

  // Smart constructors
  def mkUsername(value: String): Option[PrivateUsername] =
    (value.nonEmpty).guard[Option].as(PrivateUsername(value))

  def mkEmail(value: String): Option[PrivateEmail] =
    (value.contains("@")).guard[Option].as(PrivateEmail(value))

  // This constructors are functions which take a raw value and return a optional validated one. The optionality can be triggered by an Option, Either, Validated a.o.

  (mkUsername("Samuel"), mkEmail("samuel@samuelgomez.co")).mapN(
    (username, email) => lookupPrivateValueClass(username, email)
  )

  // Can we mess up now? YES
  (
    mkUsername("aeinstein"),
    mkEmail("aeinstein@research.com")
  ).mapN {
    case (username, email) =>
      lookupPrivateValueClass(username.copy(value = ""), email)
  }

  // Unfortunately, we are still using case classes, which means the copy method is still there. A proper way to finally get around this issue is to use sealed abstract case classes.
  // Solution? sealed abstract case class  or sealed abstract class (val ...)

  /**
    * THIS ENCODING WIL MITIGATE THE ISSUE AT COST OF MEMORY ALLOCATION, AN EVEN BETTER SOLUTION IS USE NEWTYPES LIBRARY AND DISREGARD ALL OF THIS
    */
}

object Newts {
  // It uses macros, for which we need the macro paradise compiler plugin in Scala versions below 2.13.0,
  // and only an extra compiler flag -Ymacro-annotations in versions 2.13.0 and above.
  import io.estatico.newtype.macros._

  @newtype case class Username(value: String)
  @newtype case class Email(value: String)

  // By default this removes the copy method, however, we can still do wrong. Meaning, @newtypes do not solve validation, they're just zero cost wrappers

  Email("Foo")
}

object RefinementTypes {
  import eu.timepit.refined.types.string.NonEmptyString
  import eu.timepit.refined.api.Refined
  import eu.timepit.refined.collection.Contains
  import io.estatico.newtype.macros._

  def lookup(username: NonEmptyString): IO[Option[User]] =
    ??? // Committing to IO effect to avoid creating trait and passing down F[_]

  // Here we're saying that username MUST be a NonEmptyString, however we can refine types (i.e only accept strings that contain letter 'g'
  type User = String Refined Contains['g']

  def lookupG(username: User): IO[Option[User]] =
    ???

  import eu.timepit.refined.auto._

  // lookupG("") // error
  // lookupG("aeinstein") // error
  lookupG("cyberbulletg") // compiles

  // Refinement types allow to define custom validation rules. Though, in many cases, a simple rule applies to many possible types
  // We can combine Refinement types with NewTypes
  @newtype case class Brand(value: NonEmptyString)
  @newtype case class Category(value: NonEmptyString)

  val brand: Brand = Brand("foo")
  // val category: Category = Category("") //error

}
