package linearfn

case class Wrap[T](v: T)

@main def main() = {
  println("Linear Function Library")
  case class Person(name: String, age: Int):
    def combine(other: Person): Person =
      Person(s"${this.name} & ${other.name}", this.age + other.age)

  extension [D <: Tuple](p: RestrictedSelectable.Restricted[Person, D])
    def combine[D2 <: Tuple](other: RestrictedSelectable.Restricted[Person, D2]): RestrictedSelectable.Restricted[Person, Tuple.Concat[D, D2]] =
      p.stageCall[Person, Tuple.Concat[D, D2]]("combine", Tuple1(other))

  val person1 = Person("Alice", 30)
  val person2 = Person("Bob", 25)
  val result = RestrictedSelectable.LinearFn.apply((person1, person2))(refs =>
    val wrapped = Wrap(refs._1.combine(refs._2))
    val option = Option(refs._1.combine(refs._2))
    (option, refs._2)
  )
  println(result)
}