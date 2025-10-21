package linearfn

case class Person1(name: String, age: Int):
  def method(): String = s"$name is $age years old"
case class Person2(name: String, id: Int):
  def method(): String = s"$name has id $id"

@main def main() = {
  println("runs")

  // selectable
  val person1 = Person1("Alice", 30)
  val person2 = Person2("Bob", 25)

  val ret = RestrictedSelectable.LinearFn.apply((person1, person2))(refs =>
//    println(s"method access: ${refs._1.method()}")  // doesn't work
    println(s"field access: ${refs._1.age}")  // works
    val testRef: RestrictedSelectable.Restricted[Person1, Person1, (0, 0)] = RestrictedSelectable.Restricted.LinearRef[Person1, Person1, (0, 0)](person1, x => x)
    (testRef, refs._2)
  )
  println(s"ret selectable: $ret")

  // dynamic
//  val ret2 = RestrictedDynamic.LinearFn.apply((10, "string"))(refs =>
//    (refs._1 + 1, refs._2 + refs._1)
//  )
//  println(s"ret dynamic: $ret2")
}