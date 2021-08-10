package runtime
import cats.data.ValidatedNel
import cats.implicits._
import eu.timepit.refined.api.Refined
import eu.timepit.refined.collection.NonEmpty
import eu.timepit.refined.types.string.NonEmptyString

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
}
