# LinearFn - Linear Functions in Scala 3

A Scala 3 library for enforcing **linearity** at compile time at the function level.

We define linearity as: for a function with n arguments and n return values, each argument can appear maximum once per return value but must be present in at least one return value.

## Usage

```scala
case class Person(name: String, age: Int):
  def greet(): String = s"Hello, I'm $name"
  def combine(other: Person): Person =
    Person(s"${this.name} & ${other.name}", this.age + other.age)

val alice = Person("Alice", 30)
val bob = Person("Bob", 25)

// pick implementation
import RestrictedDynamicMacros.LinearFn  // or RestrictedDynamic or RestrictedSelectable

// Define a linear function with two arguments
val result = LinearFn.apply((alice, bob))(refs =>
  val greeting = refs._1.greet()
  val age = refs._2.age
  (greeting, age)
)
// result: (Hello, I'm Alice, 25)

// ✓ This compiles - each argument used exactly once
RestrictedDynamicMacros.LinearFn.apply((alice, bob))(refs =>
  (refs._1.name, refs._2.name)
)

// ✗ This fails to compile - refs._2 not used
RestrictedDynamicMacros.LinearFn.apply((alice, bob))(refs =>
  (refs._1.name, refs._1.age)
)

// ✗ This fails to compile - field 'typo' does not exist
RestrictedDynamicMacros.LinearFn.apply((alice, bob))(refs =>
  (refs._1.name, refs._2.typo)
)

// ✗ This fails to compile because the first return value is not linear, the second is ok 
RestrictedDynamicMacros.LinearFn.apply((alice, bob))(refs =>
  (refs._1.age + refs._1.age, refs._2.age + 10)
)
```

## Implementations

This library provides three implementations of linear functions:

### 1. RestrictedDynamic

Uses Scala's `Dynamic` trait for runtime method dispatch.

- **Not type safe** - field/method names are resolved at runtime
- **Works with all types** - no restrictions on input types

```scala
import RestrictedDynamic.LinearFn

val result = LinearFn.apply((alice, bob))(refs =>
  val name = refs._1.name        // Runtime dispatch
  val badField = refs._1.typo    // Compiles, fails at runtime :(
  (name, refs._2.age)
)
```

### 2. RestrictedSelectable

Uses Scala 3's `Selectable` trait with structural types.

- **Type safe** - field/method names checked at compile time
- **Only works with product types** (case classes, tuples, etc.)
- **Requires users to define extension methods** - leaks the abstraction

```scala
import RestrictedSelectable.{LinearFn, ~}

// Users must define extension methods for each field/method
extension [D <: Tuple](p: Person ~ D)
  def name: String ~ D = p.stageField("name")
  def age: Int ~ D = p.stageField("age")
  def greet(): String ~ D = p.stageCall[String, D]("greet", EmptyTuple)

val result = LinearFn.apply((alice, bob))(refs =>
  val name = refs._1.name        // Compile-time checked
  val badField = refs._1.typo    // Compile error!
  (name, refs._2.age)
)
```

**TODO:** Confirm whether this can be made nicer to avoid leaking the abstraction.

### 3. RestrictedDynamicMacros

Uses `Dynamic` trait with Scala 3 macros for compile-time verification.

- **Type safe** - field/method existence verified at compile time via macros
- **Works with all types** - no restrictions on input types
- **Requires macros** - more complex implementation

```scala
import RestrictedDynamicMacros.LinearFn

val result = LinearFn.apply((alice, bob))(refs =>
  val name = refs._1.name          // Compile-time checked via macro
  val badField = refs._1.typo      // Compile error!
  (name, refs._2.age)
)

// Method calls with Restricted arguments automatically track dependencies
val combined = LinearFn.apply((alice, bob))(refs =>
  val person = refs._1.combine(refs._2)  // Dependencies: (1, 0)
  (person, person)  // OK - person captures both dependencies
)
```

### 4. RestrictedDynamicQuotes
TODO: next up
