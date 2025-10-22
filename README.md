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
@ops // needed for Selectable-based implementation
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

- **Linear** - linearity is enforced at the type-level. There are two drawbacks: (1) arguments passed to 
    linear functions must be product types, and (2) users must define extension methods for each field/method used in the body of
    the linear function, however this allows for more flexibility since arbitrary arguments can be marked as @unrestricted.
- **Type safe** - field/method names checked at compile time

```scala
import RestrictedSelectable.{LinearFn, Restricted}

// Users must define extension methods for each field/method.
extension [D <: Tuple](p: Person ~ D)
  def name: Restricted[String, D] = p.stageField("name")
  def age: Restricted[Int, D] = p.stageField("age")
  def greet(): Restricted[String, D] = p.stageCall[String, D]("greet", EmptyTuple)

val result = LinearFn.apply((alice, bob))(refs =>
  val name = refs._1.name        // Compile-time checked
  val badField = refs._1.typo    // Compile error!
  (name, refs._2.age)
)
```

**TODO:** Confirm whether this can be made nicer to avoid leaking the abstraction.

#### Automatic Extension Generation with `@ops`

To simplify the process of defining extension methods, the library provides an `@ops` annotation that automatically generates extension methods for all methods in a class. This uses an sbt source generator that runs during compilation.

**How it works:**

1. Annotate your class with `@ops`:
```scala
@ops
case class TestPerson(name: String, age: Int):
  def greet(): String = s"Hello, I'm $name"
  def combine(other: TestPerson): TestPerson =
    TestPerson(s"${this.name} & ${other.name}", this.age + other.age)
  def withName(newName: String): TestPerson =
    TestPerson(newName, age)
```

2. During compilation, the sbt source generator automatically creates extension methods in `target/scala-3.7.3/src_managed/main/TestPersonOps.scala`:
```scala
object TestPersonOps:
  extension [D <: Tuple](p: RestrictedSelectable.Restricted[TestPerson, D])
    def greet(): RestrictedSelectable.Restricted[String, D] =
      p.stageCall[String, D]("greet", EmptyTuple)

    def combine[D1 <: Tuple](other: RestrictedSelectable.Restricted[TestPerson, D1]):
      RestrictedSelectable.Restricted[TestPerson, Tuple.Concat[D1, D]] =
      p.stageCall[TestPerson, Tuple.Concat[D1, D]]("combine", Tuple1(other))

    def withName[D1 <: Tuple](newName: RestrictedSelectable.Restricted[String, D1]):
      RestrictedSelectable.Restricted[TestPerson, Tuple.Concat[D1, D]] =
      p.stageCall[TestPerson, Tuple.Concat[D1, D]]("withName", Tuple1(newName))
```

3. Import the generated extensions in your code:
```scala
import TestPersonOps.*

val result = LinearFn.apply((person1, person2))(refs =>
  val combined = refs._1.combine(refs._2)
  (combined, combined)
)
```

**Dependency Tracking:**

All method parameters are tracked by default. Each parameter accepts `Restricted[T, D_n]` types, and their dependencies are concatenated in the result type:

```scala
def method(arg1: T1, arg2: T2): Result
// Generates:
def method[D1 <: Tuple, D2 <: Tuple](
  arg1: Restricted[T1, D1],
  arg2: Restricted[T2, D2]
): Restricted[Result, Tuple.Concat[D1, Tuple.Concat[D2, D]]]
```

**Implicit Conversion for Plain Values:**

Plain values are automatically converted to `Restricted[T, EmptyTuple]`, allowing you to pass both tracked and untracked values:

```scala
val result = LinearFn.apply(Tuple1(person))(refs =>
  // "Alicia" is implicitly converted to Restricted[String, EmptyTuple]
  val updated = refs._1.withName("Alicia")
  Tuple1(updated)
)
```

Since `Tuple.Concat[EmptyTuple, D] = D`, plain values don't affect the dependency type.

**The `@unrestricted` Annotation:**

Mark parameters with `@unrestricted` to exclude them from dependency tracking:

```scala
@ops
case class Example(value: String):
  def combine(tracked: Example, @unrestricted config: String): Example =
    Example(s"$value + ${tracked.value} (config: $config)")
```

Generates:
```scala
def combine[D1 <: Tuple, D2 <: Tuple](
  tracked: Restricted[Example, D1],
  config: Restricted[String, D2]
): Restricted[Example, Tuple.Concat[D1, D]]  // D2 excluded!
```

This allows `@unrestricted` parameters to be:
- Returned separately without violating linearity
- Used multiple times in the same function
- Passed as plain values without affecting type checking

**Generated Files:**

The generator creates one file per `@ops`-annotated class:
- **Location**: `target/scala-3.7.3/src_managed/main/`
- **Naming**: `{ClassName}Extensions.scala`
- **Cleanup**: Automatically removed by `sbt clean`

To avoid naming conflicts, extensions are wrapped in class-specific objects (e.g., `TestPersonOps`, `ExampleOps`).

### 2. RestrictedDynamic

Uses Scala's `Dynamic` trait for runtime method dispatch. 

- **Linear** - linearity is enforced but not customizable: any method of an argument that takes another argument is
  considered a "use" of both arguments.
- **Not type safe** - field/method names are resolved at runtime (so useless but works for demonstration purposes)
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

- **Linear** - linearity is enforced but not customizable: any method of an argument that takes another argument is 
 considered a "use" of both arguments. 
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

- **Linear** - linearity is enforced but not customizable: any method of an argument that takes another argument is
  considered a "use" of both arguments.
- **Type safe** - field/method existence verified at compile time via quotes + splices, although right now it doesn't 
 work for overloaded methods due to Select.unique, but that may be fixable.
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
1. Why don't we use `map`/graded applicative functors?
 
> Haskell doesn't have to worry about side effects. If we use a `map` function that exposes a term of the type 
 wrapped in the linear type, for example `Int`, users could pass the term to an outside function that takes `Int`. 
 Since arguments are wrapped in a custom `Restricted` type, in order to extract the underlying term to pass it to a
 (potentially side-effectful) external function, the user would need to call a `.unsafe` unwrap function (which is by 
 definition is an unsafe operation).
 Also, one of the claims is that users can write the bodies of linear functions in the same way they write non-linear
 functions. Using `map` would require users to change their programming style significantly, however there is some 
 precedent in Scala for this style and the alternative just requires ahead-of-time declaration of methods, so it's a trade-off.

2. For Selectable, `Fields` works out-of-the-box, but methods are problematic. 
    Alternatives for Selectable requiring users to declare methods ahead-of-time:

> 1. If the user could define structural refinements that could be manually applied to the Restricted type, that 
 would mean that `applyDynamic` would work for those functions, which would make things much easier.

> 2. We can use a macro annotation to generate the extension methods automatically for a given type, however the ergonomics 
 are a bit weird: either the compiler generates them and the user needs to copy them manually into their codebase, or 
 we use ScalaMeta to apply and generate these extension methods ahead-of-time. 