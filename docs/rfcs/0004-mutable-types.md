# Summary

This RFC proposes syntax and semantics for mutable types.

# Motivation

Escalier defaults to immutability.  Currently, variables can be declared as mutable and mutability checks are enforced
in the following way:
- mutable variables can be assigned to immutable variables respecting the usual subtyping rules
- mutable variables can be assigned to mutable variables such that the types of the two variables are invariant

Unfortunately, there are two very big loop hole with the current system:
- There are no mutability checks for the return type for functions or methods
- There are no mutability checks when assigning constructor arguments to instance fields

**Example:**
```ts
let id = fn<T>(x: T) -> T {
    return x;
}

type Point = {
    x: number,
    y: number,
};

let p: Point = {x: 5, y: 10};
let mut q: Point = id(p); // this is equvialent to let `mut q: Point = p;` which is not allowed
```

**Example:**
```ts
class Line {
    p1: Point;
    p2: Point;
    constructor(public p1: Point, public p2: Point) {
        this.p1 = p1;
        this.p2 = p2;
    }
}

let p1: Point = {x: 5, y: 10};
let p2: Point = {x: 10, y: 5};
let mut line: Line = new Line(p1, p2); // this should not be allowed...
line.p1.x = 0; // ...because it allows us to mutate `p1` which is supposed to be immutable
```

# Explanation

## Syntax

The `mut` keyword is currently used to declare variables that are both mutable and re-assignable.  This RFC proposes that
the `mut` keyword only be used to mark types as mutable.  The `var` keyword will be used to declare variables that are
re-assignable.  Variables that are not re-assignable will be declared using the `val` keyword instead of `let`.  This
results in the following four possible variable declarations allowing us to properly distinguish between mutability of
types and re-assignability of variables:

```ts
type Point = {
    x: number,
    y: number,
};

val a: Point = {x: 5, y: 10};       // immutable data, cannot be reassigned
val b: mut Point = {x: 5, y: 10};   // mutable data, cannot be reassigned
var c: Point = {x: 5, y: 10};       // immutable data, can be reassigned
var d: mut Point = {x: 5, y: 10};   // mutable data, can be reassigned
```

If a variable is initialized without a type, we infer the type as immutable.

```ts
val p = {x: 5, y: 10}; // inferred as `{x: 5, y: 10}`
```

## Functions

To solve the issue with return types not being checked for mutability, I propose marking types as mutable using the 
`mut` keyword.  By default, functions will return immutable types, but if a function's return type is marked as mutable
then the function will be allowed to return mutable types.

**Example:**
```ts
fn makePoint(x: number, y: number) -> mut Point {
    return {x, y};
};

val p1: Point = makePoint(5, 10);
val p2: mut Point = makePoint(5, 10);
val p3 = makePoint(5, 10); // `p3` will be inferred as `Point` even though `makePoint` returns `mut Point`
```

This means that certain functions like the `identity` function will need two implementations, one for immutable types,
and one for mutable types.  This shortcoming may be addressed in a future RFC by adding support for mutability polymorphism.

**Example:**
```ts
fn id<T>(x: T) -> T {
    return x;
}
fn mutId<T>(x: mut T) -> mut T {
    return x;
}

val p1: Point = {x: 5, y: 10};
val p2 = id(p1); // `p2` will have type `Point`

val q1: mut Point = {x: 0, y: 0};
val q2 = mutId(q1); // `q2` will have type `Point`
val q3: mut Point = mutId(q1); // `mut Point` must be specified if we want the variable to be mutable
```

The same syntax and semantics will also be applied to methods.

TODO: choose a shorthand for mutable types, e.g.
```ts
val p1: mut = makePoint(5, 10); // I like this better because it's aligns better with params in function decls
val p2 = mut makePoint(5, 10);
```

## Container Types

With the addition of mutable types, we need a way to differentiate the mutability of the container type from the mutability
of the elements within the container.  Currently, we use `T[]` for array types, but sticking with this will make it hard
to differentiate between mutable arrays and arrays of mutable elements, e.g. `mut T[]` vs `(mut T)[]`.  Instead, we will
use `Array<T>` for array types.  This aligns better with other container types like `Set<T>` and `Map<K, V>` anyways. 

```ts
type ImmutableArrayOfImmutablePoints = Array<Point>;
type ImmutableArrayOfMutablePoints = Array<mut Point>;
type MutableArrayOfImmutablePoints = mut Array<Point>;
type MutableArrayOfMutablePoints = mut Array<mut Point>;

type ImmutableSetOfImmutablePoints = Set<Point>;
type ImmutableSetOfMutablePoints = Set<mut Point>;
type MutableSetOfImmutablePoints = mut Set<Point>;
type MutableSetOfMutablePoints = mut Set<mut Point>;
```

## Classes

Instances of classes can be either mutable or immutable.  If a mutable instance is created, we want to make sure that
all of the arguments assigned to instance fields are also mutable.

This section assumes that the majority of classes will be TypeScript classes with most Escalier code opting to use structs
instead.

When creating a new instance of a class, if it's being assigned to a mutable variable, then all of the arguments passed
to the constructor must also be mutable.  This is because Escalier doesn't support partial mutability and we assume that
the instance will be holding on to references of the arguments passed to the constructor.

**Example:**
```ts
class Line {
    p1: Point;
    p2: Point;
    constructor(public p1: Point, public p2: Point) {
        this.p1 = p1;
        this.p2 = p2;
    }
}

val p1 = makePoint(5, 10);
val p2 = makePoint(10, 5);
val line1 = new Line(p1, p2); // okay because `line1` is immutable
val line2: mut = new Line(p1, p2); // not allowed because `p1` and `p2` are immutable

val q1: mut = makePoint(0, 0);
val q2: mut = makePoint(1, 1);
val line3: mut = new Line(q1, q2); // okay because `q1` and `q2` are mutable
val line4: mut = new Line(makePoint(0, 0), makePoint(1, 1)); // okay because the arguments are mutable
```

The mutability of the constructor arguments is determined by the mutability of the instance being created.  If the instance
is mutable, then the constructor arguments must also be mutable.  If the instance is immutable, then the constructor arguments
can be either mutable or immutable.

In the future we may be able to add support for partial mutability with mutability polymorphism.  The idea is that there
would be a way to add a parameter to the class whose value indicates the mutability of the instance being constructed.
This parameter would then be used to mark the constructor parameters that we want to have the same mutability as the instance.

## Subtyping

`mut T` is a subtype of `T` since a mutable value can always used in place of an immutable value.

## Codegen/Interop

The mutability of a type will be erased in the TypeScript types that appear in .d.ts.  In order to support round-tripping
between TypeScript and Escalier, we include the original Escalier type in a JSDoc comment for all types appear in in
.d.ts files.

**Example:**
```ts
type Point = {
    x: number,
    y: number,
};

// @esctype - Point
export const a: Point;
// @esctype - mut Point
export const b: Point;
// @esctype - Point
export let c: Point;
// @esctype - mut Point
export let d: Point;
```
