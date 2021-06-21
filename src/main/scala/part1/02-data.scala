/** ENUMS
  *
  * Scala 3 adds support for "enums", which are to sealed traits like case classes were to classes. Enums cut down on
  * the boilerplate required to use the "sealed trait" pattern for modeling sum types, in a fashion very similar to how
  * case classes cut down on the boilerplate required to use classes to model product types.
  *
  * Scala 3 enums are not the same as Java enums: while the constructors of enums are finite, and defined statically at
  * compile-time in the same file, these constructors may have parameters, and therefore, the total number of values of
  * any enum type could be large or infinite
  */
package enums:
  enum DayOfWeek:
    case Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday

  def javaEnumInterop: Array[DayOfWeek] = DayOfWeek.values

  def sunday: DayOfWeek =
    DayOfWeek.Sunday
    DayOfWeek.valueOf("Sunday")

  enum Planet(mass: Double, radius: Double):
    private final val G                  = 6.67300e-11
    def surfaceGravity                   = G * mass / (radius * radius)
    def surfaceWeight(otherMass: Double) = otherMass * surfaceGravity

    case Mercury extends Planet(3.303e+23, 2.4397e6)
    case Venus extends Planet(4.869e+24, 6.0518e6)
    case Earth extends Planet(5.976e+24, 6.37814e6)
    case Mars extends Planet(6.421e+23, 3.3972e6)
    case Jupiter extends Planet(1.9e+27, 7.1492e7)
    case Saturn extends Planet(5.688e+26, 6.0268e7)
    case Uranus extends Planet(8.686e+25, 2.5559e7)
    case Neptune extends Planet(1.024e+26, 2.4746e7)
  end Planet

  // Emulate sealed trait of sealed traits
  //  sealed trait Event
  //  sealed trait UserEvent extends Event
  //  sealed trait DeviceEvent extends Event
  enum Event:
    case User(user: UserEvent)
    case Device(device: DeviceEvent)

  enum UserEvent:
    case Registered(id: String)
    case Anonymous

  enum DeviceEvent:
    case Device(id: String)

  // automatic support for variance
  enum Result[+Error, +Value]:
    case Succeed(value: Value)
    case Fail(error: Error)

  val succeeded: Result[Nothing, Int]  = Result.Succeed(1)
  val errored: Result[String, Nothing] = Result.Fail("BOOM!")

  enum EOption[+A]:
    case Some(x: A)
    case None

  val some1: EOption[Int]    = EOption.Some(1)
  val none: EOption[Nothing] = EOption.None

  enum Workflow[-Input, +Output]:
    case End(value: Output)

  val workflowEnd: Workflow[Any, String] = Workflow.End("YAY")

package case_classes:
  final case class Email private (value: String)
  object Email:
    def fromString(v: String): Option[Email] =
      if isValidEmail(v) then Some(Email(v))
      else None

    private def isValidEmail(v: String): Boolean = v.matches("^[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,6}$")
  end Email

  def usageEmail =
    //Email("LOL")  // won't compile
    val optEmail: Option[Email] = Email.fromString("cal@wh.com")
    // optEmail.map(email => email.copy(v = "INVALID"))  // you cannot copy either :D
    println("DONE")
