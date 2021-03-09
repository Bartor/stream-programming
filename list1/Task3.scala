import scala.util.Try
import scala.util.Success
import scala.util.Failure
object Task3 {
  def main(args: Array[String]): Unit = {
    args.foreach(arg =>
      Try(arg.toInt) match {
        case Success(i) => println(s"${i} : ${greatestDivisor(i)}")
        case Failure(s) => println(s"${arg} : Cannot be converted")
      }
    )
  }

  def greatestDivisor(number: Int): Int = {
    for (i <- 2 to Math.sqrt(number).toInt + 1) {
      if (number % i == 0) return number / i
    }
    1
  }
}
