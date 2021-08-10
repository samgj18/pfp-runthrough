package runtime
import cats.data.{EitherNel, ValidatedNel}
import cats.implicits._
import eu.timepit.refined.api.RefType.refinedRefType
import eu.timepit.refined.api.{Refined, RefinedTypeOps, Validate}
import eu.timepit.refined.collection.{Contains, NonEmpty}
import eu.timepit.refined.refineV
import eu.timepit.refined.types.string.NonEmptyString
import io.estatico.newtype.macros._
import runtime.NewtypeRefinedOps._

object RuntimeValidation {
  // Almost every application needs to deal with runtime validation.
  // For example, we can not possibly know what values we are going to receive from HTTP requests or any other service, so compile-time validation is not an option here.
  import eu.timepit.refined._
  val str: String = "some runtime value"
  val res: Either[String, NonEmptyString] =
    refineV[NonEmpty](str)

  // Most refinement types provide a 'from' method which is a convenient way of expressing the same as the above with better syntax
  // The from method helps with type inference better than the generic refineV, so it's encouraged to use this one instead
  val fromRes: Either[String, NonEmptyString] =
    NonEmptyString.from(str)

  // We can add the same feature to any custom refinement type too.
  import eu.timepit.refined.api.RefinedTypeOps
  import eu.timepit.refined.numeric.Greater

  type GTFive = Int Refined Greater[5]
  object GTFive extends RefinedTypeOps[GTFive, Int]

  val number: Int = 33

  val gtfRes: Either[String, GTFive] = GTFive.from(number)

  /**
    * Refined let us do Runtime Validation via Either (Monad). This means is a fail fast behaviour, it'll stop
    * as soon as he finds an error. If we want to accumulate errors we would need to go for cats.data.Validated, which
    * is similar to Either but it forms an Applicative (accumulate errors).
    */
  case class MyType(a: NonEmptyString, b: GTFive)

  def validate(a: String, b: Int): ValidatedNel[String, MyType] = {
    (NonEmptyString.from(a).toValidatedNel, GTFive.from(b).toValidatedNel)
      .mapN(MyType.apply)
  }

  validate("", 3)
  //  Invalid(
  //    NonEmptyList(Predicate isEmpty() did not fail.,
  //    Predicate failed: (3 > 5).)
  //  )

  // A similar result can be achieved via EitherNel + parMapN
  def validateEither(a: String, b: Int): EitherNel[String, MyType] =
    (
      NonEmptyString.from(a).toEitherNel,
      GTFive.from(b).toEitherNel
    ).parMapN(MyType.apply)

  // Left(
  //   NonEmptyList(Predicate isEmpty() did not fail.,
  //   Predicate failed: (3 > 5).)
  // )

}

object PersonDomain {
  type UserNameR = NonEmptyString
  object UserNameR extends RefinedTypeOps[UserNameR, String]

  type NameR = NonEmptyString
  object NameR extends RefinedTypeOps[NameR, String]

  type EmailR = String Refined Contains['@']
  object EmailR extends RefinedTypeOps[EmailR, String]

  @newtype case class UserName(value: UserNameR)
  @newtype case class Name(value: NameR)
  @newtype case class Email(value: EmailR)

  case class Person(
      username: UserName,
      name: Name,
      email: Email
  )

  // To perform validation, we will need an extra map to lift the refinement type
  // into our @newtype, in addition to EitherNel:
  def mkPerson(
      u: String,
      n: String,
      e: String
  ): EitherNel[String, Person] =
    (
      UserNameR.from(u).toEitherNel.map(UserName.apply),
      NameR.from(n).toEitherNel.map(Name.apply),
      EmailR.from(e).toEitherNel.map(Email.apply)
    ).parMapN(Person.apply)

  // This is a bit repetitive and comes with a little of boilerplate. The pattern can be abstracted using the Coercible typeclass
  // fom the @newtype library:

  def mkPersonCoercible(
      u: String,
      n: String,
      e: String
  ): EitherNel[String, Person] = {
    (
      validate[UserName](u),
      validate[Name](n),
      validate[Email](e)
    ).parMapN(Person.apply)
    /*
      We could also make it work as an extension method of the raw value, though, this requires two method calls instead.
      (
        u.as[UserName].validate,
        n.as[Name].validate,
        e.as[Email].validate
      ).parMapN(Person.apply)
     */
  }

}

object NewtypeRefinedOps {
  import io.estatico.newtype.Coercible
  import io.estatico.newtype.ops._

  final class NewtypeRefinedPartiallyApplied[A] {
    def apply[T, P](raw: T)(implicit
        c: Coercible[Refined[T, P], A],
        v: Validate[T, P]
    ): EitherNel[String, A] =
      refineV[P](raw).toEitherNel.map(_.coerce[A])
  }

  def validate[A]: NewtypeRefinedPartiallyApplied[A] =
    new NewtypeRefinedPartiallyApplied[A]
}
