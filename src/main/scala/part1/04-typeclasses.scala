package part1

/** TYPECLASSES
  *
  * Scala 3 introduces direct support for typeclasses using contextual features of the language.
  *
  * Typeclasses provide a way to abstract over similar data types, without having to change the inheritance hierarchy of
  * those data types, providing the power of "mixin" interfaces, but with additional flexibility that plays well with
  * third-party data types.
  */
object typeclass_basics:
  // typeclass
  trait PrettyPrint[-A]:
    extension (a: A) def prettyPrint: String

  // instances
  // anonymous
  given PrettyPrint[String] with
    extension (a: String) override def prettyPrint: String = a

  // name your instances
  given intPrettyPrint: PrettyPrint[Int] with
    extension (a: Int) override def prettyPrint: String = a.toString

  // derivation
  given listPrettyPrint[A](using p: PrettyPrint[A]): PrettyPrint[List[A]] with
    extension (a: List[A]) override def prettyPrint: String = a.map(_.prettyPrint).mkString("[", ", ", "]")

  def prettyPrintUsage: String =
    "foo".prettyPrint
    1.prettyPrint

  def prettyPrintTypeclassMethod[A](in: A)(using p: PrettyPrint[A]): String =
    in.prettyPrint

// Given = Provide some proof
// Using = Require some proof

  // Alias Givens
  import scala.concurrent.ExecutionContext
  given globalEc: ExecutionContext = ExecutionContext.global

object given_scopes:
  trait Hash[-A]:
    extension (a: A) def hash: Int

  object Hash:
    given Hash[String] = _.hashCode
    given Hash[Int]    = _.hashCode
    given Hash[Long]   = _.hashCode
    given Hash[Float]  = _.hashCode
    given Hash[Double] = _.hashCode

  object givens_examples:
    // import all givens into scope
    import given_scopes.Hash.given
    12.123.hash

  object givens_piecemeal_examples:
    import given_scopes.Hash.given_Hash_Int // convention if you do not explicitly name
    12.hash
    import given_scopes.Hash.given_Hash_Double
    12.123.hash

  object usings:
    def hashingIntsAndDoubles(using Hash[Int], Hash[Double]) =
      12.hash
      12.123.hash

// Automatic Conversions
object conversion:
  final case class Rational(n: Int, d: Int)
  final case class Decimal(d: BigDecimal)
  final case class American(odds: Int)

  given Conversion[Rational, Decimal] =
    r => Decimal(1 + (r.n.toDouble / r.d))

  given Conversion[Rational, American] =
    r =>
      American(
        if (r.n > r.d) ((r.n.toDouble / r.d) * 100).toInt
        else (-100 / (r.n.toDouble / r.d)).toInt
      )

@main
def conversionExample =
  import conversion._

  import scala.language.implicitConversions
  val american: American = Rational(1, 4)
  val decimal: Decimal   = Rational(3, 4)
  println(american)
  println(decimal)
