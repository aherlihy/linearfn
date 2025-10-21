package linearfn

case class Person(name: String, age: Int):
  def greet(): String = s"Hello, I'm $name"
  def getAge(): Int = age

@main def main() = {
  println("runs")

  val person1 = Person("Alice", 30)
  val person2 = Person("Bob", 25)

  val ret = RestrictedSelectable.LinearFn.apply((person1, person2))(refs =>
    // Body looks like regular code
    val age1 = refs._1.age  // Field access via Selectable - stages the computation
    (age1, refs._2)
  )

  println(s"Results after execution: $ret")

  // dynamic
//  val ret2 = RestrictedDynamic.LinearFn.apply((10, "string"))(refs =>
//    (refs._1 + 1, refs._2 + refs._1)
//  )
//  println(s"ret dynamic: $ret2")
}