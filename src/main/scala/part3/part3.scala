package part3

/** Scala 3 introduces context functions (?=>), which are functions that depend on some context.
  */
object context_functions:
  type HTML[+A] = StringBuilder ?=> A

  def append(s: String)(using b: StringBuilder): Unit = b.append(s)

  def p(text: String): HTML[Unit] =
    append(s"<p>$text</p>")

  def h1(text: String): HTML[Unit] =
    append(s"<h1>$text</h1>")

  def div[A](id: String)(html: HTML[A]): HTML[Unit] =
    append(s"<div id=$id>${makeHtml(html)}</div>")

  def makeHtml[A](html: HTML[A]): String =
    given b: StringBuilder = new StringBuilder()
    html(using b) // alternatively you can write in
    b.toString

  @main
  def example: Unit =
    println {
      makeHtml {
        h1("Hello")
        p("How are you")
        div("cal") {
          p("Nice to meet you")
        }
        p("See you!")
      }
    }

/** SINGLETON TYPES
  *
  * Literals in Scala now have their own singleton types, which are subtypes of their broader types. For example, the
  * value `true` has a subtype of `Boolean`, namely, `true`. Singleton types provide additional precision and are a
  * relatively simple change to the language that is useful in conjunction with type-level and metaprogramming.
  */
object singleton_type:
  val trueVal: true = true

  def IsSubtypeOf[A, B](using evidence: A <:< B): Unit = ()
  infix type IsSubtypeOfType[A, B >: A]

  val trueSubtypeBoolean = IsSubtypeOf[true, Boolean]
  type StringTest = "stringValue" IsSubtypeOfType String

/** TRAIT PARAMETERS
  *
  * Scala 3 introduces trait parameters, which solve a lot of messy initialization order problems in Scala 2.x.
  */
object trait_parameters:
  trait Console:
    def print(line: String): Unit

  val StandardConsole: Console = println(_)

  trait Logging(console: Console):
    def log(line: => String): Unit = console.print(line)

  class StandardLogger extends Logging(StandardConsole)

/** EXPLICIT NULLS
  *
  * When the -Yexplicit-nulls flag is turned on, Scala 3 will treat `Null` as a subtype of `Any`, and not a supertype of
  * either `AnyRef` or `AnyVal`. Nullable types are then described with union types.
  */
object explicit_nulls:
  val stringOrNull: String | Null = null

  def printOutOnlyIfString(value: String | Null): Unit =
    if value != null then
      // Scala is aware that value is no longer null - if you remove the test above, this will not work
      // It performs a type refinement here - this is an instance of flow sensitive typing
      val str: String = value
      println(str)
    else ()

/** CREATOR APPLICATIONS
  *
  * Scala 3 introduces creator applications, which is a concise way to create instances of a class even if it does not
  * have an `apply` method in its companion object.
  */
object creator_applications:
  class Logger(printer: String => Unit) {
    var enabled = true

    def log(s: => String): Unit = if (enabled) printer(s)
  }

  val logger = Logger(println(_))

/** PROXIES
  *
  * Scala 3 introduces "proxies", otherwise known as _export clauses_. Export clauses can automatically create
  * forwarders to the members on another object.
  */
object proxies:
  trait Logger:
    def log(line: String): Unit

  val ConsoleLogger: Logger = println(_)

  class MyConsole(logger: Logger):
    def readLine(): String        = scala.io.StdIn.readLine()
    def printLine(any: Any): Unit = println(any.toString())
    export logger.log // or logger._ for everything

  val exampleConsole = MyConsole(ConsoleLogger)
  exampleConsole.log("Hello!")

/** PARAMETER UNTUPLING
  */
object param_untupling:
  val sum = (x: Int, y: Int) => x + y

  val numbers1 = List(1, 2, 3, 4)
  val numbers2 = List(4, 3, 2, 1)

  numbers1.zip(numbers2).map(sum)
