package linearfn

import RestrictedSelectable.~

// Define Person normally
case class Person(name: String, age: Int):
  def greet(): String = s"Hello, I'm $name"
  def getAge(): Int = age
  def combine(other: Person): Person =
    Person(s"${this.name} & ${other.name}", this.age + other.age)

// Force the user to declare methods they are going to use - not very nice.
extension [D <: Tuple](p: Person ~ D)
  def greet(): String ~ D = p.stageCall[String, D]("greet", EmptyTuple)
  def getAge(): Int ~ D = p.stageCall[Int, D]("getAge", EmptyTuple)
  def combine[D2 <: Tuple](other: Person ~ D2): Person ~ (Tuple.Concat[D, D2]) =
    p.stageCall[Person, Tuple.Concat[D, D2]]("combine", Tuple1(other))

@main def main() = {
  println("runs")

  val person1 = Person("Alice", 30)
  val person2 = Person("Bob", 25)

  val ret = RestrictedSelectable.LinearFn.apply((person1, person2))(refs =>
    // Body looks like regular code
    val age1 = refs._1.combine(refs._2)  // Field access via Selectable - stages the computation
    (age1, refs._2)
  )

  println(s"Results after execution: $ret")

  // dynamic
//  val ret2 = RestrictedDynamic.LinearFn.apply((10, "string"))(refs =>
//    (refs._1 + 1, refs._2 + refs._1)
//  )
//  println(s"ret dynamic: $ret2")
}