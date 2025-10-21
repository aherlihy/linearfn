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
    (refs._1, refs._2)
  )
  println(s"ret selectable: $ret")

  // dynamic
//  val ret2 = RestrictedDynamic.LinearFn.apply(("string", 20))(refs =>
////    refs._1 + refs._2
//    (refs._2, refs._1)
//  )
//  println(s"ret dynamic: $ret2")
}