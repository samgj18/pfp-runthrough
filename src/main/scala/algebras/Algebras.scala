package algebras

object Algebras {
  // An Algebra describes a new language (DLS - Domain Specific Language - MetaCircular - Structure and Interpretation of Computer Programs (SICP)), in this case, Scala

  // Tagless Final encodings have little to do with typeclasses.

  /**
    * We can break the definition into its constituent parts: Final and Tagless.  The use of final is to contrast with the typical initial encoding of a language. Let’s start by looking at an initial encoding.
    */

  // Example

  // This is Tagless Final encoded algebra. Tagless Algebra for short: A simple interface that abstracts over the effect type using a type constructor F[_]. Do not
  // confuse with typeclasses, which in Scala, happen to share the same encoding.
  /**
    * The difference is that typeclasses should have coherent instances, whereas tagless algebras could have many implementations, or more commonly called interpreters.
    * Tagless algebras should not have typeclass constraints
    */
  trait Counter[F[_]] {
    def incr: F[Unit]
    def get: F[Int]
  }

}
