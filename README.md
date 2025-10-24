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

#### Automatic Extension Generation with `@ops`

To simplify the process of defining extension methods, the library provides an `@ops` annotation that automatically generates extension methods for all methods in a class. This uses an sbt source generator that runs during compilation.

**Setup for Library Users:**

To use `@ops` in your own project, you'll need to set up the sbt source generator in your `build.sbt`:

```scala
// Add dependency on linearfn library
libraryDependencies += "com.yourorg" %% "linearfn" % "0.1.0"

// Add source generator for @ops annotations
Compile / sourceGenerators += Def.task {
  val sourceDir = (Compile / scalaSource).value
  val targetDir = (Compile / sourceManaged).value
  val log = streams.value.log

  linearfn.OpsExtensionGenerator.generate(sourceDir, targetDir, log)
}.taskValue
```

**Note:** The `@ops` annotation is a simple marker annotation (not a macro annotation), so no `-experimental` compiler flag is required. The actual code generation happens during the sbt compilation phase via the source generator.

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

**Parameter Tracking Annotations:**

The `@ops` generator supports three parameter tracking modes:

| Annotation | Applied To | Tracking Behavior | Use Case |
|------------|-----------|-------------------|----------|
| _(none)_ | Parameters | Full tracking | Default - track parameter and its dependencies |
| `@unrestricted` | Parameters | No tracking | Pure values, config, predicates that don't need tracking |
| `@restrictedFn` | Function parameters | Track return type only | Higher-order functions - track what function returns, not function itself |
| `@consumed` | Methods | Marks method as consuming receiver | Terminal operations like `close()`, `freeze()` |
| `@unconsumed` | Methods | Method works on any consumption state | Query methods like `size()` that don't mutate |

**Detailed Tracking Modes:**

1. **Default (no annotation)** - Full parameter tracking:
   ```scala
   def combine(other: Example): Example
   // Generates: other: Restricted[Example, D1, C1]
   // Dependencies: tracked and concatenated
   ```

2. **`@unrestricted`** - No tracking:
   ```scala
   def combine(@unrestricted config: String): Example
   // Generates: config: String
   // Dependencies: not tracked at all
   ```

   Use `@unrestricted` when parameters:
   - Should not affect linearity checking
   - Can be used multiple times
   - Are pure values or configuration

3. **`@restrictedFn`** - Track function return type only:
   ```scala
   def flatMap[B](@restrictedFn f: A => Query[B]): Query[B]
   // Generates: f: A => Restricted[Query[B], D1, C1]
   // Dependencies: only the returned Query[B] is tracked
   ```

   Use `@restrictedFn` for higher-order functions where:
   - The function itself is a callback/transformer
   - You want to track what the function returns, not the function object
   - Common in DSLs with `map`/`flatMap`/`filter` operations

**Examples:**

```scala
@ops
case class Query[A](data: List[A]):
  // Default: tracks the entire function parameter
  def transform(f: A => Query[A]): Query[A] =
    Query(data.flatMap(a => f(a).data))

  // @restrictedFn: only tracks the Query[B] returned by f
  def flatMap[B](@restrictedFn f: A => Query[B]): Query[B] =
    Query(data.flatMap(a => f(a).data))

  // @unrestricted: doesn't track the function at all
  def map[B](@unrestricted f: A => B): Query[B] =
    Query(data.map(f))

  // @unrestricted: doesn't track config parameter
  def filter(@unrestricted predicate: A => Boolean): Query[A] =
    Query(data.filter(predicate))

// Usage:
val result = LinearFn.apply((q1, q2))(refs =>
  // flatMap allows returning refs._2 because only return type is tracked
  val combined = refs._1.flatMap(x => refs._2)

  // map doesn't track the function at all
  val mapped = refs._1.map(x => x * 2)

  (combined, mapped)
)
```

**`@restrictedFn` Restrictions:**

The `@restrictedFn` annotation can ONLY be used on single-parameter function types (`A => B`):

```scala
// ✓ VALID: Single-parameter function
def flatMap[B](@restrictedFn f: A => Query[B]): Query[B]

// ✗ INVALID: Multi-parameter function
def combine(@restrictedFn f: (A, B) => C): C
// Error: "@restrictedFn can only be used on single-parameter functions"

// ✗ INVALID: Non-function parameter
def process(@restrictedFn config: String): Result
// Error: "@restrictedFn can only be used on function parameters"
```

These errors are caught during sbt source generation and reported as build warnings.

**Generated Files:**

The generator creates one file per `@ops`-annotated class:
- **Location**: `target/scala-3.7.3/src_managed/main/`
- **Naming**: `{ClassName}Extensions.scala`
- **Cleanup**: Automatically removed by `sbt clean`

To avoid naming conflicts, extensions are wrapped in class-specific objects (e.g., `TestPersonOps`, `ExampleOps`).

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

val result = LinearFn.apply((1, 2))(refs =>
  val a1 = refs._1.toDouble          // Compile-time checked via macro
  val badField = refs._1.typo      // Compile error!
  (a1, refs._2)
)

// Method calls with Restricted arguments automatically track dependencies
val combined = LinearFn.apply((1, 2))(refs =>
  val add = refs._1 + refs._2 // Dependencies: (1, 0)
  (add, add)  // OK - person captures both dependencies
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
1. Why don't we use `map`/graded applicative functors to wrap linear types?
 
> Haskell doesn't have to worry about side effects. If we use a `map` function that exposes a term of the type 
 wrapped in the linear type, for example `Int`, users could pass the term to an outside function that takes `Int`. 
 Since arguments are wrapped in a custom `Restricted` type, in order to extract the underlying term to pass it to a
 (potentially side-effectful) external function, the user would need to call a `.unsafe` unwrap function (which is by 
 definition is an unsafe operation, so the user is knowingly circumventing the linearity check).
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

3. Passing non-restricted terms to methods of Restricted types in the body of linear functions (e.g., refs._1.method(5))

> This is possible, for example if you want to pass an integer to a method of a Restricted type.
 Implicit conversions from plain values to `Restricted[T, EmptyTuple]` (for Selectable so far) allow users
 to pass plain values to methods that expect `Restricted` arguments. Because the dependency type of the conversion 
 is `EmptyTuple`, it doesn't affect linearity tracking.

4. Returning types that nest Restricted types, e.g., `List[Restricted[T, D]]`.

 > **Built-in containers** like `List`, `Option`, and `Vector` are supported out-of-the-box. When returned from a linear
 function, `List[Restricted[T, D]]` is automatically lifted into `Restricted[List[T], D]`. This is handled by match types
 in `LinearFnBase.scala` and works at arbitrary nesting depths (`List[List[Restricted[T, D]]]`, etc.).

 > **User-defined containers** can be made liftable by:
 > 1. Implementing the `Liftable[F[_]]` functor trait for your container type
 > 2. Calling `.lift` when returning wrapped Restricted values

 > Implicit conversions won't help here because the linearity match types will fail before implicits are considered.
 >
 > Example:
 > ```scala
 > case class Box[T](contents: T)
 >
 > given RestrictedSelectable.Restricted.Liftable[Box] with
 >   def map[A, B](fa: Box[A])(f: A => B): Box[B] = Box(f(fa.contents))
 >
 > val result = LinearFn.apply((ex1, ex2))(refs =>
 >   val boxed = Box(refs._1).lift  // Explicit .lift call
 >   (boxed, refs._2)
 > )
 > ```

5. **Consumption Tracking with `@consumed` and `@unconsumed` Annotations:**

Some operations conceptually "consume" an object, after which further operations shouldn't be allowed. Examples include:
 - `close()` on a file handle
 - `freeze()` on a mutable array
 - `commit()` or `rollback()` on a transaction

Our linearity system now tracks consumption state using a third type parameter `C <: Tuple` on `Restricted[A, D, C]`:
- `C = EmptyTuple`: Value is unconsumed (can call regular operations)
- `C = Tuple1[true]`: Value is consumed (limited operations allowed)

**Three Method Annotations:**

1. **Default (no annotation)**: Requires unconsumed receiver, returns unconsumed value
   ```scala
   def write(i: Int, a: A): MArray[A]
   // Generated:
   // extension [D <: Tuple](p: Restricted[MArray[A], D, EmptyTuple])
   //   def write(...): Restricted[MArray[A], ..., EmptyTuple]
   ```

2. **`@consumed`**: Requires unconsumed receiver, returns consumed value
   ```scala
   @consumed
   def freeze(): Array[A]
   // Generated:
   // extension [D <: Tuple](p: Restricted[MArray[A], D, EmptyTuple])
   //   def freeze(): Restricted[Array[A], D, Tuple1[true]]
   ```

3. **`@unconsumed`**: Accepts any consumption state, preserves that state
   ```scala
   @unconsumed
   def size(): (MArray[A], Int)
   // Generated:
   // extension [D <: Tuple, C <: Tuple](p: Restricted[MArray[A], D, C])
   //   def size(): Restricted[(MArray[A], Int), D, C]
   ```

**Example Usage:**

```scala
@ops
case class MArray[A](private val buf: Array[A]):
  // Default: unconsumed → unconsumed
  def write(i: Int, a: A): MArray[A] = { buf(i) = a; this }

  // @unconsumed: any → any (preserves state)
  @unconsumed
  def size(): (MArray[A], Int) = (this, buf.length)

  // @consumed: unconsumed → consumed
  @consumed
  def freeze(): Array[A] = buf.clone()

// ✓ OK: write, then freeze
LinearFn.apply(Tuple1(arr))(refs =>
  val updated = refs._1.write(0, 10)  // EmptyTuple → EmptyTuple
  val frozen = updated.freeze()       // EmptyTuple → Tuple1[true]
  Tuple1(frozen)
)

// ✓ OK: size can be called before freeze
LinearFn.apply(Tuple1(arr))(refs =>
  val (arr1, sz) = refs._1.size()     // Unconsumed → unconsumed
  val frozen = arr1.freeze()          // Unconsumed → consumed
  Tuple1(frozen)
)

// ✓ OK: size can be called after freeze (because @unconsumed)
LinearFn.apply(Tuple1(arr))(refs =>
  val consumed = refs._1.freeze()     // Unconsumed → consumed
  val (arr2, sz) = consumed.size()    // Consumed → consumed (@unconsumed preserves)
  Tuple1(arr2)
)

// ✗ ERROR: Can't write to consumed value
LinearFn.apply(Tuple1(arr))(refs =>
  val consumed = refs._1.freeze()     // Unconsumed → consumed
  val updated = consumed.write(0, 10) // ERROR: write requires EmptyTuple
  Tuple1(updated)
)

// ✗ ERROR: Can't freeze twice
LinearFn.apply(Tuple1(arr))(refs =>
  val frozen = refs._1.freeze()       // Unconsumed → consumed
  val frozen2 = frozen.freeze()       // ERROR: freeze requires EmptyTuple
  Tuple1(frozen2)
)
```

**Returning Consumed Values:**

Use `applyConsumed` to require that all arguments are consumed:

```scala
val result = LinearFn.applyConsumed(Tuple1(arr))(refs =>
  val frozen = refs._1.freeze()       // C = Tuple1[true]
  Tuple1(frozen)                      // OK: frozen is consumed
)

// ERROR: write doesn't consume, so this fails
LinearFn.applyConsumed(Tuple1(arr))(refs =>
  val updated = refs._1.write(0, 10)  // C = EmptyTuple (not consumed!)
  Tuple1(updated)                     // ERROR: must be consumed
)
```

**Argument Consumption (Future Work):**

Currently, method parameters ignore consumption state - both consumed and unconsumed arguments are accepted. For example:
```scala
// Both work, even though other has different consumption states
def combine(other: MArray[A]): MArray[A]
```

This may be refined in future versions to allow consuming or requiring specific consumption states for arguments.

6. **Relationship to Traditional Linear Types:**

> Our definition of linearity is closer to scoped/region-based linear types than linear Haskell's "lineary on function types". 
>
> - **Traditional linear types**: Values can be consumed without appearing in output
>  ```haskell
>  close :: File %1-> IO ()  -- Consumes file, returns nothing
>  ```
>
> - **Our current system**: All inputs must contribute to output (stricter)
>  ```scala
>  // ERROR: refs._1 not used (no dependency in output)
>  LinearFn.apply(Tuple1(file))(refs => Tuple1(42))
>  ```
>
> - **With `@consumed`**: Adds state tracking orthogonal to linearity
>- **Linearity** = dependency appears in output types (unchanged)
> - **Consumed** = operations no longer available on this value (new)
>
> Both would be enforced simultaneously. Consumed values still must contribute to the output.