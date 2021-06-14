package part2

import scala.annotation.tailrec

/** INTERSECTION TYPES
  *
  * Scala 3 introduces intersection types, which are a commutative version of the `with` operator. In Scala 3, `A & B`
  * is the same type as `B & A`. Whereas, `A with B` is only the same as `B with A` in the event there are no overlaps
  * between `A` and `B`.
  *
  * Intersection types are useful to describe types having all the members of other types.
  *
  *   - Commutativity: A & B == B & A
  *   - Associativity: (A & B) & C == A & (B & C)
  *   - A & Nothing == Nothing Distributivity: A &
  *   - (B | C) == A & B | A & C
  */

object intersection_types:
  final case class User(name: String, id: String, email: String)

  trait HasLogging:
    def logging: Logging

  final case class Logging(log: String => Unit)
  val TestLogging: Logging = Logging(println(_))

  trait HasUserRepo:
    def userRepo: UserRepo

  final case class UserRepo(getUserById: String => User)
  val TestUserRepo: UserRepo = UserRepo(_ => User("Sherlock Holmes", "sholmes", "sherlock@holmes.com"))

  type HasLoggingAndUserRepo = HasLogging & HasUserRepo

  def theSameAs[A, B](using ev: A =:= B) = ()

  // order does not matter :) ==> commutativity
  theSameAs[HasLogging & HasUserRepo, HasUserRepo & HasLogging]

  /** NOTE: Intersection types A & B replace compound types A with B in Scala 2.
    *
    * For the moment, the syntax A with B is still allowed and interpreted as A & B, but its usage as a type (as opposed
    * to in a new or extends clause) will be deprecated and removed in the future.
    */
  theSameAs[HasLogging with HasUserRepo, HasUserRepo & HasLogging] // Commutativity

  class BothUserRepoAndLogging extends HasLogging, HasUserRepo:
    def logging: Logging   = TestLogging
    def userRepo: UserRepo = TestUserRepo

  val both: HasLoggingAndUserRepo = BothUserRepoAndLogging()

  type YouCantDefineAnyValuesOfThisType = Int & String

/** UNION TYPES
  *
  * Scala 3 introduces union types, which have no direct analogue in Scala 2.x. The union of two types `A` and `B`,
  * written `A | B`, describes the type of values that have either type `A` or type `B`. For example, `Int | String` is
  * the type of values that have either type `Int` or type `String`. Union types are powerful but do have limitations
  * stemming from type erasure.
  *
  *   - Commutativity: A | B == B | A
  *   - Associativity: A | (B | C) == (A | B) | C
  *   - Identity: A | Nothing == A
  *   - forall B >: A: A | B == B
  */
object union_types:
  final case class PaymentDenied(message: String)
  final case class MissingAddress(message: String)
  final case class NetworkError(message: String)

  type PaymentDeniedOrMissingAddress = PaymentDenied | MissingAddress

  val example1: PaymentDeniedOrMissingAddress = PaymentDenied("No cash for you")
  val example2: PaymentDeniedOrMissingAddress = MissingAddress("Ain't got no place to call home")

  example2 match
    case PaymentDenied(m)  => println(m)
    case MissingAddress(m) => println(m)

  type SomeList = List[String] | List[Int]

//  // When you use union types - you have to type case to distinguish between the types in the uinion
//  // This is subject to type erasure
//  def whatList(l: SomeList) = l match
//    // compiler gives you a warning about this
//    // the type test for List[String] cannot be checked at runtime
//    // Always prints Strings
//    case strings: List[String] => println("Strings")
//
//    // the type test for List[String] cannot be checked at runtime
//    case ints: List[Int] => println("ints")
//
//  @main
//  def experimentWhatList =
//    whatList(List.empty[Int])

/** MATCH TYPES
  *
  * Match types bring the `match` construct to the type level, allowing the creation of type-level functions that return
  * different types depending on the (statically known) input types.
  */
object match_types:
  def theSameAs[A, B](using ev: A =:= B) = ()

  type Combine[Left, Right] =
    Left match
      case Unit =>
        Right

      case ? => // ? is wildcard
        Right match
          case Unit => Left
          case ?    => (Left, Right)

  val unitAndString: Combine[Unit, String] = "Hello"
  val stringAndUnit: Combine[String, Unit] = "Hello"
  val stringAndString: Combine[String, String] = ("Hello", "world")

  /**
   * EXAMPLE
   *
   * On the JVM, collection types generally "box" primitive values like `Int`, creating wrapper
   * values around these primitives. One of the exceptions is `Array`, which does not box primitive
   * values.
   *
   * Create a match type that will return Scala's `Vector` for all types except primitive types,
   * but for primitive types, will return Scala's `Array`.
   */
  type Collection[X] =
    X match
      case String => Array[String]
      case Int => Array[Int]
      case Float => Array[Float]
      case Double => Array[Double]
      case Short => Array[Short]
      case Byte => Array[Byte]
      case ? => Vector[?]

  val ints: Collection[Int] = Array.emptyIntArray
  theSameAs[Collection[Int], Array[Int]]

  /**
   * EXAMPLE
   *
   * Match types can be recursive. Write a match type that determines the "atom" type of a string
   * or array or iterable.
   */
  type ElementType[X] = X match
    case String => Char
    case Array[t] => ElementType[t]
    case Iterable[t] => ElementType[t]
    case AnyVal => X

  @tailrec
  def headOf[X](x: X): ElementType[X] =
    x match
      case s: String => s.charAt(0)
      case a: Array[t] => headOf(a.head)
      case a: Iterable[t] => headOf(a.head)
      case o: AnyVal => o

  // Match types do not have to be 'total'
  type Partial[X] =
    X match
      case String => Float
      case Float => String

  // This will not compile
  // existential type ==> unknowable
  // no way to construct a value without resorting to asInstanceOf
  // def partialInt: Partial[Int] = 1
  // i.e. you cannot use any other than String/Float

  type Bigger[A] =
    A match
      case Float => Double
      case Int => Long
      case A => A   // There are limitations to using ? versus the type A

  def testPolyParam[A](a: A): Bigger[A] =
    a match
      case a: Float => a.toDouble
      case a: Int => a.toLong
      case a: A => a

/**
 * OPAQUE TYPES
 *
 * Opaque types are a new variant of a type alias that hides information on the underlying type.
 * This can be useful to create novel types that are backed by other types, without any runtime
 * overhead. An example might be an "Email" type that is really backed by a "String", but which is
 * treated as a unique (opaque) type by the Scala compiler.
 */
object opaque_types:
  object email_example:
    opaque type Email = String
    object Email:
      /**
       * The companion object scope for an opaque type has special privileges.
       * Here the compiler remembers that Email = String
       */
      def apply(email: String): Email = email

      extension (e: Email) def username: String = e.takeWhile(_ != '@')
    end Email

  import email_example._
  def printString(string: String): Unit = println(string)
  val exampleEmail: Email = Email("admin@caesars.us")


// compiler has forgotten that Email === String so the following won't compile
  // printString(exampleEmail)

  object email_example_subtype:
    // Add type bounds to leak information
    // Every Email2 is a String (the reverse is not true - Every String is not an Email2)
    opaque type Email2 <: String = String
    object Email2:
      def apply(email: String): Email2 = email

  import email_example_subtype._
  // wherever a String is used - you can use an Email2 instead
  printString(Email2("this@canbe.used"))

  def printEmail(e: Email2): Unit = println(e)
  //printEmail("reverse is not true")
