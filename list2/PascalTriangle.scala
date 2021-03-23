import scala.util.Try
import scala.util.Failure
import scala.util.Success

class PascalTriangle(val n: Int) {
  if (n < 1) throw new IndexOutOfBoundsException();

  private val row = {
    var currentRow = List(1)
    for (i <- 0 to n) {
      currentRow = currentRow :+ (currentRow(i) * (n - i) / (i + 1));
    }
    currentRow
  }

  def number(m: Int): Int = row(m);
}

object Main {
  def main(args: Array[String]): Unit = {
    Try(args.head.toInt) match {
      case Failure(exception) => {
        println("Incorrect argument");
      }
      case Success(value) => {
        Try(new PascalTriangle(value)) match {
          case Failure(exception) => println(f"${value} - out of range")
          case Success(primeNumbers) => {
            args.tail.foreach(arg => {
              Try(primeNumbers.number(arg.toInt)) match {
                case Failure(exception: IndexOutOfBoundsException) =>
                  println(f"${arg} - index out of range")
                case Failure(exception) => println(f"${arg} - invalid arg")
                case Success(value)     => println(f"${arg} - ${value}")
              }
            });
          }
        }
      }
    }
  }
}
