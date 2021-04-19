import scala.util.Try
import scala.util.Success
import scala.util.Failure

abstract class Figure {
  def perimeter(): Double
  def area(): Double
}

class Circle(private val radius: Double) extends Figure {
  def perimeter(): Double = Math.PI * radius * radius

  def area(): Double = 2 * Math.PI * radius
}

class Pentagon(private val side: Double) extends Figure {
  def perimeter(): Double = 5 * side

  def area(): Double = 1 / 4 * side * side * Math.sqrt(25 + 20 * Math.sqrt(5))
}

class Hexagon(private val side: Double) extends Figure {
  def perimeter(): Double = 6 * side

  def area(): Double = 3 / 2 * side * side * Math.sqrt(3)
}

abstract class Quadrangle(protected val sides: List[Double]) extends Figure {
  def perimeter(): Double = sides.sum
}

class Square(side: Double) extends Quadrangle(List(side, side, side, side)) {
  def area(): Double = sides(0) * sides(0)
}

class Rectangle(sideA: Double, sideB: Double)
    extends Quadrangle(List(sideA, sideB, sideA, sideB)) {
  def area(): Double = sides.distinct(0) * sides.distinct(1)
}

class Rhombus(side: Double, private val angle: Double)
    extends Quadrangle(List(side, side, side, side)) {
  def area(): Double = sides(0) * sides(0) * Math.sin(angle * Math.PI / 180)
}

object Main {
  def main(args: Array[String]): Unit = {
    if (args.length == 1) {
      print("Not enough arguments")
      return;
    }

    var currentIndex = 0
    val sides = Try(args.tail.map(_.toDouble).filter(_ > 0)) match {
      case Success(value) => value
      case Failure(exception) => {
        print("Illegal argument")
        return;
      }
    }
    val figures = Array.ofDim[Figure](args.head.size)
    for ((c, i) <- args.head.zipWithIndex) {
      c match {
        case 'c' => {
          Try(
            sides.slice(currentIndex, currentIndex + 1).head
          ) match {
            case Success(value)     => figures(i) = new Circle(value)
            case Failure(exception) => println("Not enough arguments")
          }
          currentIndex = currentIndex + 1
        }
        case 'q' => {
          Try(
            sides.slice(currentIndex, currentIndex + 5)
          ) match {
            case Success(value) => {
              (value.init.distinct.size, value.last) match {
                case (1, 90) => figures(i) = new Square(value.tail.distinct(0))
                case (1, angle) =>
                  figures(i) = new Rhombus(value.tail.distinct(0), angle)
                case (2, 90) =>
                  if (value.tail.count(_ == value.tail.distinct(0)) != 2)
                    println("Incorrect sides for a right-angle quadrangle")
                  else
                    figures(i) = new Rectangle(
                      value.tail.distinct(0),
                      value.tail.distinct(1)
                    )
                case _ => println("Incorrect sides")
              }
            }
            case Failure(exception) => println("Not enough arguments")
          }
          currentIndex = currentIndex + 5
        }
        case 'p' => {
          Try(
            sides
              .slice(
                currentIndex,
                currentIndex + 1
              )
              .head
          ) match {
            case Success(value)     => figures(i) = new Pentagon(value)
            case Failure(exception) => println("Not enough arguments")
          }

          currentIndex = currentIndex + 1
        }
        case 's' => {
          Try(
            sides
              .slice(
                currentIndex,
                currentIndex + 1
              )
              .head
          ) match {
            case Success(value)     => figures(i) = new Hexagon(value)
            case Failure(exception) => println("Not enough arguments")
          }

          currentIndex = currentIndex + 1
        }
      }
    }

    for (figure <- figures) {
      if (figure != null)
        println(s"Area: ${figure.area()}, perimeter: ${figure.perimeter()}")
    }
  }
}
