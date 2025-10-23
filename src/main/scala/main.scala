package linearfn

case class Wrap[T](v: T)

// Make Wrap liftable by providing a Liftable instance
given RestrictedSelectable.Restricted.Liftable[Wrap] with
  def map[A, B](fa: Wrap[A])(f: A => B): Wrap[B] = Wrap(f(fa.v))



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

  // Test 1: Wrap with Liftable instance should work now!
  val result1 = RestrictedSelectable.LinearFn.apply((person1, person2))(refs =>
    val wrapped = Wrap(refs._1.combine(refs._2)).lift
    (wrapped, refs._2)
  )
  println(s"Wrapped result: ${result1._1.v}")

  // Test 2: Option (built-in) still works
  val result2 = RestrictedSelectable.LinearFn.apply((person1, person2))(refs =>
    val option = Option(refs._1.combine(refs._2))
    (option, refs._2)
  )
  println(s"Option result: ${result2._1}")
}