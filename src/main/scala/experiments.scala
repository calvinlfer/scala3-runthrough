import scala.compiletime._
import zio.prelude._

package safer:
  type Digit5OrMore = Digit5OrMore.Type
  object Digit5OrMore extends Subtype[Int]:
    private inline def check(i: Int): Boolean      = i >= 5
    private inline def renderError(i: Int): String = s"input $i must be at least 5"

    inline def compiletime(inline i: Int): Digit5OrMore =
      inline if check(i) then Digit5OrMore(i)
      else error(renderError(i))

    def runtime(i: Int): Either[String, Digit5OrMore] =
      if check(i) then Right(Digit5OrMore(i))
      else Left(renderError(i))

  val compileTimeExample: Digit5OrMore             = Digit5OrMore.compiletime(5)
  val runtimeExample: Either[String, Digit5OrMore] = Digit5OrMore.runtime(5)
