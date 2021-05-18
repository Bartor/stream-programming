import scala.collection.mutable.Map
import scala.collection.mutable.Set
import scala.util.Random

class Car(val brand: String) {}

class Book(val title: String) {}

class Employee(val name: String) {}

object LossyCounting {
  def lossyCounting(
      epsilon: Double,
      stream: Stream[AnyRef]
  ): Map[String, (Int, Set[(String, Int, Int)])] = {
    var sets: Map[String, (Int, Set[(String, Int, Int)])] = Map()
    val w = (1 / epsilon).toInt

    stream.zipWithIndex.foreach {
      case (element, i) => {
        val className = element.getClass.getSimpleName

        val (bCurrent, set) =
          sets.getOrElseUpdate(className, (1, Set()))

        def matchCase(key: String) = set.find(_._1 == key) match {
          case Some((x, f, delta)) => {
            set -= ((x, f, delta))
            set += ((x, f + 1, delta))
          }
          case None => {
            set += ((key, 1, bCurrent - 1))
          }
        }

        element match {
          case element: Car      => matchCase(element.brand)
          case element: Book     => matchCase(element.title)
          case element: Employee => matchCase(element.name)
        }

        if (i % w == 0) {
          val newSet = set.filter { case (x, f, delta) => f + delta > bCurrent }
          sets(className) = (bCurrent + 1, newSet)
        }
      }
    }

    sets
  }

  def main(args: Array[String]) {
    val inputStream: Stream[AnyRef] = Random
      .shuffle(
        List[AnyRef](
          new Car("BMW"),
          new Car("Mercedes"),
          new Car("Mercedes"),
          new Car("Mercedes"),
          new Car("BMW"),
          new Car("BMW"),
          new Car("BMW"),
          new Car("BMW"),
          new Car("Opel"),
          new Car("Opel"),
          new Car("Opel"),
          new Employee("Nowak"),
          new Employee("Nowak"),
          new Book("Cool Book"),
          new Book("Uncool Book"),
          new Book("Cool Book")
        )
      )
      .to(Stream)

    print(lossyCounting(0.3, inputStream))
  }
}
