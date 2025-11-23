# RestrictedFn - Modular Substructural Constraints in Scala 3

A Scala 3 library for enforcing flexible substructural constraints at compile time at the function level without modifying the compiler
itself, using type-level programming with Match, Tuple, and Union types, and staging.
Oftentimes, traditional substructural constraints like linear types are too restrictive for real-world use-cases that
require a combination of different substructural constraints. This library allows users to tune the constraints to their
specific use-case and defines a clear API for declaring function behavior.
The techniques are designed for writing embedded DSLs in host programming languages with side effects, like Scala.
Example case studies are located in `test/casestudies`.

## Substructural Constraints

This library provides a `RestrictedFn` function that enforces substructural constraints specified by the user through custom tensors (connectives).

**Substructural Type Systems** restrict the structural rules of logic (weakening, contraction, exchange):
- **Linear types**: Use exactly once (no weakening, no contraction)
- **Affine types**: Use at most once (weakening allowed, no contraction)
- **Relevant types**: Use at least once (contraction allowed, no weakening)

**Our Implementation:**

Users can define custom tensors using `ComposedConnective` that combine two orthogonal dimensions of constraints:
- **ForAll**: Applied across all return values collectively
- **ForEach**: Applied to each return value individually

Each dimension can have a multiplicity constraint:
- `Multiplicity.Linear`: Use exactly once
- `Multiplicity.Affine`: Use at most once
- `Multiplicity.Relevant`: Use at least once
- `Multiplicity.Unrestricted`: No restrictions

For functions with _n_ arguments and _m_ return values, each argument flows through the function to the return values. Users can specify constraints for each return value (ForEach), for all return values collectively (ForAll), or a combination of both. This allows for more flexible constraints than traditional substructural type systems.

Parameters of methods can be annotated to customize tracking:
- Default: Track full parameter and its dependencies
- `@unrestricted`: Do not track parameter at all
- `@restrictedReturn`: For function parameters, only track return type (useful for higher-order functions)

### Using Custom Tensors (ComposedConnectives) for Different Constraint Combinations

You can wrap return values in different custom tensor types to select constraint combinations.
The library provides helper types for common patterns:

```scala
import linearfn.RestrictedSelectable.{RestrictedFn, *}
import test.{ForAllLinearConnective, ForAllAffineConnective, ForAllRelevantConnective}

// Traditional linear types (each arg used exactly once total)
// ForAll = Linear, ForEach = Unrestricted
RestrictedFn.apply((a, b))(refs =>
  ForAllLinearConnective(Tuple1(refs._1, refs._2))  // OK: each used once
  // ForAllLinearConnective((refs._1, refs._2, refs._1)) // Error: refs._1 used twice
)

// Affine (each arg used at most once across all returns)
// ForAll = Affine, ForEach = Unrestricted
RestrictedFn.apply((a, b))(refs =>
  ForAllAffineConnective((refs._1, refs._2))  // OK
  // ForAllAffineConnective((refs._1, refs._1)) // Error: refs._1 used twice
)

// Relevant (each arg used at least once across all returns)
// ForAll = Relevant, ForEach = Unrestricted
RestrictedFn.apply((a, b))(refs =>
  ForAllRelevantConnective((refs._1, refs._2))  // OK: both used
  // ForAllRelevantConnective(Tuple1(refs._1)) // Error: refs._2 not used
)
```

**Connective Helper Types** (defined in test/TestUtils.scala as examples):
```scala
// ForAll multiplicity, ForEach unrestricted
type ForAllLinearConnective[RT <: Tuple] =
  ComposedConnective[RT, Multiplicity.Unrestricted, Multiplicity.Linear]

type ForAllAffineConnective[RT <: Tuple] =
  ComposedConnective[RT, Multiplicity.Unrestricted, Multiplicity.Affine]

type ForAllRelevantConnective[RT <: Tuple] =
  ComposedConnective[RT, Multiplicity.Unrestricted, Multiplicity.Relevant]
```

**Multiplicity Constraints**:
- `Multiplicity.Linear`: Used exactly once
- `Multiplicity.Affine`: Used at most once
- `Multiplicity.Relevant`: Used at least once
- `Multiplicity.Unrestricted`: No restrictions

**Constraint Dimensions**:
- **ForAll**: Applied across all return values collectively
- **ForEach**: Applied to each return value individually

## Usage

```scala
import linearfn.RestrictedSelectable.{RestrictedFn, *}
import test.ForAllAffineConnective  // Example helper type

@ops // needed for Selectable-based implementation
case class Person(name: String, age: Int):
  def greet(): String = s"Hello, I'm $name"
  def combine(other: Person): Person =
    Person(s"${this.name} & ${other.name}", this.age + other.age)

val alice = Person("Alice", 30)
val bob = Person("Bob", 25)

// Define a linear function with two arguments
val result = RestrictedFn.apply((alice, bob))(refs =>
  val greeting = refs._1.greet()
  val age = refs._2.age
  ForAllAffineConnective((greeting, age))
)
// result: (Hello, I'm Alice, 25)

// ✓ This compiles - each argument used exactly once
RestrictedFn.apply((alice, bob))(refs =>
  ForAllAffineConnective((refs._1.name, refs._2.name))
)

// ✗ This fails to compile - refs._2 not used (violates Relevant constraint)
RestrictedFn.apply((alice, bob))(refs =>
  ForAllRelevantConnective((refs._1.name, refs._1.age))
)

// ✗ This fails to compile - field 'typo' does not exist
RestrictedFn.apply((alice, bob))(refs =>
  ForAllAffineConnective((refs._1.name, refs._2.typo))
)

// ✗ This fails to compile because refs._1 used twice (violates Affine constraint)
RestrictedFn.apply((alice, bob))(refs =>
  ForAllAffineConnective((refs._1.age + refs._1.age, refs._2.age + 10))
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
import linearfn.RestrictedSelectable.{RestrictedFn, Restricted, *}
import test.ForAllAffineConnective

// Users must define extension methods for each field/method (or use @ops)
object PersonOps:
  extension [D <: Tuple](p: Restricted[Person, D])
    def name: Restricted[String, D] = p.stageField("name")
    def age: Restricted[Int, D] = p.stageField("age")
    def greet(): Restricted[String, D] = p.stageCall[String, D]("greet", EmptyTuple)

import PersonOps.*

val result = RestrictedFn.apply((alice, bob))(refs =>
  val name = refs._1.name        // Compile-time checked
  val badField = refs._1.typo    // Compile error!
  ForAllAffineConnective((name, refs._2.age))
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
import linearfn.RestrictedSelectable.RestrictedFn
import test.ForAllAffineConnective

val result = RestrictedFn.apply((person1, person2))(refs =>
  val combined = refs._1.combine(refs._2)
  ForAllAffineConnective((combined, combined))
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
| `@unrestricted` | Parameters | No tracking                                                               | Pure values, config, predicates that don't need tracking                                                   |
| `@restrictedReturn` | Function parameters | Track return type only                                                    | Higher-order functions - track what function returns, not function itself                                  |

**Detailed Tracking Modes:**

1. **Default (no annotation)** - Full parameter tracking:
   ```scala
   def combine(other: Example): Example
   // Generates: other: Restricted[Example, D1]
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

3. **`@restrictedReturn`** - Track function return type only:
   ```scala
   def flatMap[B](@restrictedReturn f: A => Query[B]): Query[B]
   // Generates: f: A => Restricted[Query[B], D1]
   // Dependencies: only the returned Query[B] is tracked
   ```

   Use `@restrictedReturn` for higher-order functions where:
   - The function itself is a callback/transformer
   - You want to track what the function returns, not the function object
   - Common in DSLs with `map`/`flatMap`/`filter` operations

**Parameter Tracking Examples:**

```scala
@ops
case class Query[A](data: List[A]):
  // Default: tracks the entire function parameter
  def transform(f: A => Query[A]): Query[A] =
    Query(data.flatMap(a => f(a).data))

  // @restrictedReturn: only tracks the Query[B] returned by f
  def flatMap[B](@restrictedReturn f: A => Query[B]): Query[B] =
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

**`@restrictedReturn` Restrictions:**

The `@restrictedReturn` annotation can ONLY be used on single-parameter function types (`A => B`):

```scala
// ✓ VALID: Single-parameter function
def flatMap[B](@restrictedReturn f: A => Query[B]): Query[B]

// ✗ INVALID: Multi-parameter function
def combine(@restrictedReturn f: (A, B) => C): C
// Error: "@restrictedReturn can only be used on single-parameter functions"

// ✗ INVALID: Non-function parameter
def process(@restrictedReturn config: String): Result
// Error: "@restrictedReturn can only be used on function parameters"
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
 All operations on linear types are tracked through the dependency tuples. However, this does mean that the library
 is more useful for DSLs than for general-purpose programming, since staging can introduce repeated computations
 if users call side-effectful methods on non-restricted types. 

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