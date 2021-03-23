import scala.util.Try
import scala.util.Failure
import scala.util.Success

class PrimeNumbers(val n: Int) {
  if (n < 2) throw new IndexOutOfBoundsException();

  private def sieve(list: LazyList[Int]): LazyList[Int] =
    list.head #:: sieve(list.tail).filter(_ % list.head != 0);

  private val primes: LazyList[Int] = sieve(LazyList.from(2)).takeWhile(_ < n);

  def number(m: Int): Int = primes(m);
}

object Main {
  def main(args: Array[String]): Unit = {
    Try(args.head.toInt) match {
      case Failure(exception) => {
        println("Incorrect argument");
      }
      case Success(value) => {
        Try(new PrimeNumbers(value)) match {
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
