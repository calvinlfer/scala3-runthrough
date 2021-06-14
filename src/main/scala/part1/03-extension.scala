/** EXTENSION METHODS
  *
  * Scala 3 brings first-class support for "extension methods", which allow adding methods to classes after their
  * definition. Previously, this feature was emulated using implicits.
  */
package part1:

  final case class Email(value: String)
  extension (e: Email) def username: String = e.value.takeWhile(_ != '@')

  def emailUsage =
    val sherlock = Email("sherlock@holmes.com")
    println(sherlock.username)

  extension [A, B](optA: Option[A])
    def zip(optB: Option[B]): Option[(A, B)] =
      for
        a <- optA
        b <- optB
      yield (a, b)

  extension (s: String) def isSherlock: Boolean = s.startsWith("Sherlock")

  def extensionStringUsage =
    "Watson".isSherlock
