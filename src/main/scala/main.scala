package linearfn


// Define Person normally
case class Person(name: String, age: Int):
  def greet(): String = s"Hello, I'm $name"
  def getAge(): Int = age
  def combine(other: Person): Person =
    Person(s"${this.name} & ${other.name}", this.age + other.age)

@main def main() = {
  val person1 = Person("Alice", 30)
  val person2 = Person("Bob", 25)

//  // Test 1: Field access via selectDynamic macro
//  println("=== Test 1: Field access ===")
//  val res1 = RestrictedDynamicMacros.LinearFn.apply((person1, person2))(refs =>
//    val nameAccess = refs._1.name
//    (nameAccess, refs._2.age)
//  )
//  println(s"Result: $res1\n")
//
//  // Test 2: Zero-arg method calls via applyDynamic macro
//  println("=== Test 2: Method calls (zero args) ===")
//  val res2 = RestrictedDynamicMacros.LinearFn.apply((person1, person2))(refs =>
//    val greeting = refs._1.greet()
//    (greeting, refs._2.getAge())
//  )
//  println(s"Result: $res2\n")

  // Test 3: Method call with Restricted argument (dependency tracking)
  // This tests that when calling refs._1.combine(refs._2), the macro correctly
  // concatenates dependencies: Tuple.Concat[Tuple1[1], Tuple1[0]] = (1, 0)
//  println("=== Test 3: Method with Restricted arg (dependency tracking) ===")
//  val res3 = RestrictedDynamicMacros.LinearFn.apply((person1, person2))(refs =>
//    val combined = refs._1.combine(refs._2)
//    (combined, combined)
//  )
//  println(s"Result: $res3")
}