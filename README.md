# LinearFn - Parametric Substructural Constraints in Scala 3

A Scala 3 library for enforcing flexible substructural constraints at compile time at the function level without modifying the compiler
itself, using type-level programming with Match, Tuple, and Union types, and staging.
Oftentimes, traditional substructural constraints like linear types are too restrictive for real-world use-cases that
require a combination of different substructural constraints. This library allows users to tune the constraints to their
specific use-case and defines a clear API for declaring function behavior.
The techniques are designed for writing embedded DSLs in host programming languages with side effects, like Scala.
Example case studies are located in `test/casestudies`.

## Substructural Constraints 

This library provides a `restrictedFn` library function that enforces the substructural constraints specified by the user.

1. **Traditional substructural types**: 
    Users pass the desired multiplicity (linear, affine, relevant) to `restrictedFn` and declare the types of the 
    function parameters using the `@ops` annotation. The library enforces the specified constraints at compile time.
    The definition of "use" is also customizable by the user on a per-method basis: the default behavior of methods of 
    types declared with `@ops` is equivalent to the `@consumed` annotation, indicating that a method consumes or "uses"
    the receiver.
    Within the scope of a `restrictedFn`, a `@consumed` method may be called according to the constraint level specified by the user.
    This is the classical understanding of "use" of a value within a region (the function body),
    useful for resource management (file handles must be closed exactly once, transactions must be committed or rolled 
    back exactly once, etc.). Users can also customize methods as `@repeatable` (receiver must be unconsumed, but does 
    not consume the receiver, for example `isClosed`); or `@unconsumed` (receiver can be unconsumed or consumed, does 
    not change consumption state, useful for debugging or helper methods). 

2. **Multi-argument, multi-return-value functions**: For functions with _n_ arguments and _m_ return values,
    each argument flows through the function to the return values. Users can specify constraints for _each_ return-value, 
    allowing for more flexible constraints that are not possible with traditional substructural type systems, or
    for _all_ return values. For example, a user may want to enforce that each argument is used at least once across all return values (affine),
    but at most once in each individual return value (relevant). Parameters of methods of argument types can be declared as 
    `@restricted` (default behavior), `@unrestricted`, or `@restrictedFn` (for function parameters, only track return type,
    useful for higher-order-functions).

### Relationship to Substructural Type Systems

**Substructural Type Systems** restrict the structural rules of logic (weakening, contraction, exchange):
- **Linear types**: Use exactly once (no weakening, no contraction)
- **Affine types**: Use at most once (weakening allowed, no contraction)
- **Relevant types**: Use at least once (contraction allowed, no weakening)

**Our Implementation:**

1. **C Parameter = Defines constraints on a single value**
   - Models resource lifecycle with two states: unconsumed and consumed
   - Default methods consume the receiver
   - `@repeatable` methods allow multiple uses
   - `@unconsumed` methods preserve state

2. **D Parameter = Defines constraints on combinations of multiple values**
   - Per-return-value constraints
   - For-all-return-values constraints
   - `@restricted` parameters (default) track full parameter
   - `@unrestricted` parameters do not track parameter
   - `@restrictedFn` parameters track return type only (for higher-order functions)
     
    | Combination | What It Allows                                                      | Example Valid______________                                  | Example Invalid                                                                    | Use Case                                                                                                                  |
     |-------------|---------------------------------------------------------------------|--------------------------------------------------------------|------------------------------------------------------------------------------------|---------------------------------------------------------------------------------------------------------------------------|
     | **For-all-relevant +<br>For-each-affine**<br> | Each arg appears ≥1 times across all returns,<br>≤1 time per return | `(a, b) → (a, b)`<br>`(a, b) → a + b`<br>`(a, b) → (a+b, a)` | `(a, b) → (a, a)` (not relevant)<br>`(a, b) → (a+a, b)` (not affine)               | DSL that models linear structural recursion.<br>Allows mutually-recursive programs while preventing non-linear recursion. |
     | **For-all-relevant +<br>For-all-affine** | Each arg appears ≥1 times across all returns,<br>≤1 time per return | `(a, b) → (a, b)`<br>`(a, b) → (b, a)`<br>`(a, b) → (a+b, x)` | `(a, b) → (a, b, a)` (not affine) <br>`(a, b) → (a, a)` (not relevant)             | TODO: Traditional linear types for multi-return functions.                                                                |
     | **For-each-relevant +<br>For-each-affine** | Each arg appears ≥1 times in every return,<br>≤1 time per return    | `(a, b) → (a+b, a+b)` <br/>`(a, b)→ a + b` | `(a, b) → (a, b)` (not relevant)<br>`(a, b) → (a+a, b+b)` (not affine)             | TODO                                                                                                                      |
     | **For-each-relevant +<br>For-all-affine** | Each arg appears ≥1 times in every return,<br>≤1 time total         | `(a) → (a)`<br>`(a, b) → (a+b)`                              | `(a, b) → (a, b)` (a in 1st and 2nd)<br>`(a, b) → (a+b, a+b)` (a,b in 1st and 2nd) | TODO: Requires the function to have only a single return value that uses all parameters once.                             |

**Key Insights:**

1. **For-all-relevant + For-each-affine** (current) is the most flexible:
    - Allows multiple return values of varying structure
    - Each argument must be used somewhere (ensures linearity)
    - No duplicate use within a single return (prevents aliasing)
    - Example: fixed-point with linear but not-strongly-connected dependencies.

2. **For-all-relevant + For-all-affine** is traditional linear types:
    - Each argument used exactly once across all returns
    - More restrictive but simpler to reason about
    - Closer to Linear Haskell's model

3. **For-each-relevant + For-each-affine** is very restrictive:
    - All returns must have the same dependency structure
    - Only useful for uniform recursive patterns
    - Example: fixed-point with linear strongly connected dependencies (full mutual recursion). 

4. **For-each-relevant + For-all-affine** is impractical:
    - Can't have multiple returns (would violate for-all-affine)
    - Only works for single return value

**Comparison to Related Systems:**

TODO

### Using customApply for Different Constraint Combinations

You can use `customApply` to select different constraint combinations:

```scala
import linearfn.{VerticalConstraint, HorizontalConstraint}

// Traditional linear types (each arg used exactly once total)
LinearFn.customApply(
  (vertical = VerticalConstraint.Affine,
   horizontal = HorizontalConstraint.ForAllRelevantForAllAffine)
)((a, b))(refs =>
  (refs._1, refs._2)  // OK: each used once
  // (refs._1, refs._2, refs._1) // Error: refs._1 used twice
)

// Uniform recursion (all returns must use all args)
LinearFn.customApply(
  (vertical = VerticalConstraint.Affine,
   horizontal = HorizontalConstraint.ForEachRelevantForEachAffine)
)((a, b))(refs =>
  // Both returns must contain both a and b
  (a.combine(b), a.combine(b))
)

// Vertical constraints control consumption
LinearFn.customApply(
  (vertical = VerticalConstraint.Linear,  // Must consume exactly once
   horizontal = HorizontalConstraint.ForAllRelevantForEachAffine)
)((file1, file2))(refs =>
  (refs._1.close(), refs._2.close())  // OK: consumed via close()
  // (refs._1, refs._2) // Error: not consumed
)
```

**Vertical Constraints** (consumption tracking via `C` parameter):
- `VerticalConstraint.Affine`: Consumed at most once (default)
- `VerticalConstraint.Linear`: Consumed exactly once
- `VerticalConstraint.Relevant`: Consumed at least once

**Horizontal Constraints** (dependency tracking via `D` parameter):
- `HorizontalConstraint.ForAllRelevantForEachAffine`: Current default (flexible recursion)
- `HorizontalConstraint.ForAllRelevantForAllAffine`: Traditional linear types
- `HorizontalConstraint.ForEachRelevantForEachAffine`: Uniform recursion
- `HorizontalConstraint.ForEachRelevantForAllAffine`: Single return only (impractical)

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

## Implementation

### RestrictedSelectable

Uses Scala 3's `Selectable` trait with structural types.

- **Linear** - linearity is enforced at the type-level. There are two drawbacks: (1) arguments passed to 
    linear functions must be product types, and (2) users must define extension methods for each field/method used in the body of
    the linear function, however this allows for more flexibility since arbitrary arguments can be marked as @unrestricted.
    See FAQ for discussion on alternative implementations.
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

All method parameters are tracked by default.

```scala
def method(arg1: T1, arg2: T2): Result
// Generates:
def method[D1 <: Tuple, D2 <: Tuple](
  arg1: Restricted[T1, D1],
  arg2: Restricted[T2, D2]
): Restricted[Result, Tuple.Concat[D1, Tuple.Concat[D2, D]]]
```

**Parameter Tracking Annotations:**

The `@ops` generator supports annotations to customize how methods and their parameters contribute to linearity tracking. 

| Annotation | Applied To | Tracking Behavior                                                         | Use Case                                                                                                   |
|------------|-----------|---------------------------------------------------------------------------|------------------------------------------------------------------------------------------------------------|
| _(none)_ | Parameters | Full tracking                                                             | Default - track parameter and its dependencies                                                             |
| _(none)_ | Methods | Consumes receiver                                                         | Default - terminal operations that consume the receiver                                                    |
| `@unrestricted` | Parameters | No tracking                                                               | Pure values, config, predicates that don't need tracking                                                   |
| `@restrictedFn` | Function parameters | Track return type only                                                    | Higher-order functions - track what function returns, not function itself                                  |
| `@repeatable` | Methods | Can only be called on unconsumed values but can be called multiple times. | Operations that can be chained: `write()`, `map()`, `flatMap()`                                            |
| `@consumed` | Methods | Explicit version of default - consumes receiver.                          | Terminal operations like `close()`, `freeze()` (same as no annotation)                                     |
| `@unconsumed` | Methods | Method works on any consumption state and does not modify reciever.       | Relax constraints for this particular call, good for debugging or helper methods that do not modify state. |

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

4. **`@repeatable`, `@consumed`, and `@unconsumed`** - Method consumption tracking:

   Some operations conceptually "consume" an object, after which further operations shouldn't be allowed. Examples include:
   - `close()` on a file handle
   - `freeze()` on a mutable array
   - `commit()` or `rollback()` on a transaction

   The linearity system tracks consumption state using a third type parameter `C <: Tuple` on `Restricted[A, D, C]`:
   - `C = EmptyTuple`: Value is unconsumed (can call regular operations)
   - `C = Tuple1[true]`: Value is consumed (limited operations allowed)

   **Default (no annotation)**: Requires unconsumed receiver, returns consumed value
   ```scala
   def freeze(): Array[A]
   // Generated:
   // extension [D <: Tuple](p: Restricted[MArray[A], D, EmptyTuple])
   //   def freeze(): Restricted[Array[A], D, Tuple1[true]]
   ```

   **`@repeatable`**: Requires unconsumed receiver, returns unconsumed value
   ```scala
   @repeatable
   def write(i: Int, a: A): MArray[A]
   // Generated:
   // extension [D <: Tuple](p: Restricted[MArray[A], D, EmptyTuple])
   //   def write(...): Restricted[MArray[A], ..., EmptyTuple]
   ```

   **`@consumed`**: Explicit version of default - requires unconsumed receiver, returns consumed value
   ```scala
   @consumed
   def close(): String
   // Generated:
   // extension [D <: Tuple](p: Restricted[FileHandle, D, EmptyTuple])
   //   def close(): Restricted[String, D, Tuple1[true]]
   ```

   **`@unconsumed`**: Accepts any consumption state, preserves that state
   ```scala
   @unconsumed
   def size(): (MArray[A], Int)
   // Generated:
   // extension [D <: Tuple, C <: Tuple](p: Restricted[MArray[A], D, C])
   //   def size(): Restricted[(MArray[A], Int), D, C]
   ```

**Consumption Tracking Examples:**

Example taken from Linear Haskell. Say you want a mutable array that you can write to many times, but when you call 
`freeze()`, you can no longer write to it.

```scala
@ops
case class MArray[A](private val buf: Array[A]):
  // @repeatable: unconsumed → unconsumed (can be called multiple times)
  @repeatable
  def write(i: Int, a: A): MArray[A] = { buf(i) = a; this }

  // @unconsumed: any → any (preserves state)
  @unconsumed
  def size(): (MArray[A], Int) = (this, buf.length)

  // Default: unconsumed → consumed (terminal operation)
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

Use `applyConsumed` to require that all arguments are consumed:

```scala
val result = LinearFn.applyConsumed(Tuple1(arr))(refs =>
  val frozen = refs._1.freeze()       // C = Tuple1[true] (default consumes)
  Tuple1(frozen)                      // OK: frozen is consumed
)

// ERROR: write doesn't consume (marked @repeatable), so this fails
LinearFn.applyConsumed(Tuple1(arr))(refs =>
  val updated = refs._1.write(0, 10)  // C = EmptyTuple (@repeatable doesn't consume)
  Tuple1(updated)                     // ERROR: must be consumed
)
```

**Note:** Currently, the consumption state of method parameters are ignored - both consumed and unconsumed arguments are 
accepted. This may be refined in future versions if there is a use case.

**Parameter Tracking Examples:**

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

2. How can side effects be prevented if Scala is not purely functional?
> Assuming that users do not call `.unsafe` unwrap functions in the body of a linear function, side effects are prevented 
 by (1) having all linear types wrapped in the `Restricted` type (see FAQ#1), and (2) staging the computation: 
 operations on linear types are wrapped in lambdas that are stored in the `Restricted` type. 
 Only terms returned by the linear function body are executed, so operations that are not returned are never executed.
 As users are required to annotate any methods that consume the value with @consumed, all operations on linear types 
 are tracked. However, this does mean that the library is more useful for DSLs than for general-purpose programming,
 since staging can introduce repeated computations if users call side-effectful methods on non-restricted types. 

3. Returning types that nest Restricted types, e.g., `List[Restricted[T, D]]`.

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

4. **Alternative Implementations:**

This library also provides alternative implementations based on Scala's `Dynamic` trait.

**RestrictedDynamic**

Uses Scala's `Dynamic` trait for runtime method dispatch.

- **Linear** - linearity is enforced but not customizable: any method of an argument that takes another argument is considered a "use" of both arguments.
- **Not type safe** - field/method names are resolved at runtime (so useless, but POC)
- **Works with all types** - no restrictions on input types or requirement that users annotate their classes.

```scala
import RestrictedDynamic.LinearFn

val result = LinearFn.apply((alice, bob))(refs =>
  val name = refs._1.name        // Runtime dispatch
  val badField = refs._1.typo    // Compiles, fails at runtime :(
  (name, refs._2.age)
)
```

**RestrictedDynamic + Macros**

Uses `Dynamic` trait with Scala 3 macros for compile-time type checking.

- **Linear** - linearity is enforced but not customizable: any method of an argument that takes another argument is considered a "use" of both arguments.
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

**RestrictedDynamic + Quotes**

Uses `Dynamic` trait with Scala 3 quotes & splices for AST inspection + compile-time type checking.

- **Linear** - linearity is enforced but not customizable: any method of an argument that takes another argument is considered a "use" of both arguments.
- **Type safe** - field/method existence verified at compile time via quotes + splices, although right now it doesn't work for overloaded methods due to Select.unique, but that may be fixable.
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