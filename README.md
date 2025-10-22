# LinearFn - Linear Functions in Scala 3

A Scala 3 library for enforcing **linearity** at compile time at the function level without modifying the compiler 
itself. Can be used for an embedded DSL but also regular Scala functions.

We use the following definition of linearity:
For a function with _n_ arguments and _n_ return values, each argument will be present a maximum of once **per return value** 
but must be present at least one across all return values. 
It's fairly easy to add another implementation of `linearFn` that uses the definition 
of linearity that requires that each argument appears exactly once across all return values, but we use the first 
because it's more immediately useful for our use cases.

## Usage

```scala
case class Person(name: String, age: Int):
  def greet(): String = s"Hello, I'm $name"
  def combine(other: Person): Person =
    Person(s"${this.name} & ${other.name}", this.age + other.age)

val alice = Person("Alice", 30)
val bob = Person("Bob", 25)

// Define a linear function with two arguments
val result = LinearFn.apply((alice, bob))(refs =>
  val greeting = refs._1.greet()
  val age = refs._2.age
  (greeting, age)
)
// result: (Hello, I'm Alice, 25)

// ✓ This compiles - each argument used exactly once
LinearFn.apply((alice, bob))(refs =>
  (refs._1.name, refs._2.name)
)

// ✗ This fails to compile - refs._2 not used
LinearFn.apply((alice, bob))(refs =>
  (refs._1.name, refs._1.age)
)

// ✗ This fails to compile - field 'typo' does not exist
LinearFn.apply((alice, bob))(refs =>
  (refs._1.name, refs._2.typo)
)

// ✗ This fails to compile because the first return value is not linear, the second is ok 
LinearFn.apply((alice, bob))(refs =>
  (refs._1.age + refs._1.age, refs._2.age + 10)
)
```

## Implementations

This library provides three implementations of linear functions, each with different trade-offs.


### 1. RestrictedSelectable

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

### 2. RestrictedDynamic

Uses Scala's `Dynamic` trait for runtime method dispatch.

- **Not type safe** - field/method names are resolved at runtime
- **Works with all types** - no restrictions on input types or requirement that users annotate their classes.

```scala
import RestrictedDynamic.LinearFn

val result = LinearFn.apply((alice, bob))(refs =>
  val name = refs._1.name        // Runtime dispatch
  val badField = refs._1.typo    // Compiles, fails at runtime :(
  (name, refs._2.age)
)
```
### 2A. RestrictedDynamic + Macros

Uses `Dynamic` trait with Scala 3 macros for compile-time type checking.

- **Type safe** - field/method existence verified at compile time via macros
- **Works with all types** - no restrictions on input types
- **Requires macros** - more complex implementation, requires reflection.

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

### 2B. RestrictedDynamic + Quotes 

Uses `Dynamic` trait with Scala 3 quotes & splices for AST inspection + compile-time type checking.

- **Type safe** - field/method existence verified at compile time via macros
- **Works with all types** - no restrictions on input types
- **Requires quotes** - more complex implementation.

```scala
import RestrictedDynamicQuotes.LinearFn

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

## FAQ
1. Why don't we use `map` or other "graded applicative functors"?
 > Haskell doesn't have to worry about side effects. If we use a `map` function that exposes a term of the type 
 wrapped in the linear type, for example `Int`, users could pass terms to outside functions that accept the `Int` type. 
 Since arguments are wrapped in a custom `Restricted` type, in order to extract the underlying type to pass it to an
 (potentially side-effectful) external function, the user would need to call `.unwrap` which is by definition an unsafe 
 operation.