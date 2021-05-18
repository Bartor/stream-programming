import scala.collection.mutable.Map
import scala.util.Random

class Car(val brand: String) {}

class Book(val title: String) {}

class Employee(val name: String) {}

object SpaceSaving {
  def spaceSaving(
      k: Int,
      stream: LazyList[AnyRef]
  ): Map[String, Map[String, Int]] = {
    var maps: Map[String, Map[String, Int]] = Map()

    stream.foreach(element => {
      val map = maps.getOrElseUpdate(element.getClass.getSimpleName, Map())

      def matchCase(key: String) = map.get(key) match {
        case Some(value) => map(key) = value + 1
        case None => {
          if (map.keySet.size < k - 1) { map(key) = 1 }
          else {
            val (minKey, minValue) = map.minBy(_._2)
            map.remove(minKey)
            map += (key -> (minValue + 1))
          }
        }
      }

      element match {
        case element: Car      => matchCase(element.brand)
        case element: Book     => matchCase(element.title)
        case element: Employee => matchCase(element.name)
      }
    })

    maps
  }

  def main(args: Array[String]) {
    val inputStream: LazyList[AnyRef] = Random
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
      .to(LazyList)

    print(spaceSaving(3, inputStream))
  }
}
