package new_syntax:

  // Scala 3 makes braces optional
  class ClassDecl:
    def run() = println("Hello World!")

  trait TraitDecl:
    def run() = println("Hello World!")

  object ObjectBody:
    def run() = println("Hello World!")

  def conditional() =
    if 2 + 2 != 4 then throw new IllegalStateException("The universe is broken")

  def conditional2() =
    if "Sherlock Holmes".startsWith("Sher") then println("He is sure!")
    else println("He is uncertain!")

  def patmat(v: String) =
    v match
      case "knock, knock" => println("Who's there?")
      case _              => println("Unknown input!")

  def multiline(): Unit =
    println("What is your name?")
    val name = scala.io.StdIn.readLine()
    println(s"Hello, ${name}!")

  def multilineWithMarker(): Unit =
    println("What is your name?")
    val name = scala.io.StdIn.readLine()
    println(s"Hello, ${name}!")
  end multilineWithMarker

  def tryCatch =
    try
      println("Let's go!")
      throw new IllegalStateException("OH NOES!")
    catch
      case _: IllegalStateException =>
        println("That state is illegal y'all")
        println("We are done")

package control_flow:
  def conditional(x: Int) =
    if x > 0 then println("Positive")
    else println("Non-positive")

  def whileDoExample(n: Int)(body: () => Unit): Unit =
    var i = 0
    while i < n do
      body()
      i = i + 1

  def forComprehension: List[Int] =
    val numbers = List(1, 2, 9, 3, -1, 6, 5, 2)
    for
      i <- numbers
      j <- numbers
    yield i * j
