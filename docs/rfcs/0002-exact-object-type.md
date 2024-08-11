# Summary

Exact object are not allowed to have extra properties that aren't present in
their inferred types.

# Motivation

Inexact object types couples with structural typing make it very easy for objects
to end up with extra properties. Even if we implement excess property checking,
that only happens with object literals and extra properties can still creep in.

```ts
type Point = { x: number; y: number }; // current, inexact object type
let pointAndFlag = { x: 5, y: 10, flag: true };
let point: Point = pointAndFlag; // allowed with inexect object types
```

Having extra properties in an object can also lead to objects ending up with
the incorrect values in them.

```ts
type Point = { x: number; y: number }; // current, inexact object type

let pointAndFlag1 = { x: 5, y: 10, flag: true };
let point: Point = pointAndFlag1; // allowed with inexect object types

type PointAndFlag = { x: number; y: number; flag?: string };
let pointAndFlag2: PointAndFlag = point;
```

`pointAndFlag2.flag` is typed as an optional `string`, but it contains a `boolean`
value.

# Explanation

## Syntax

```ts
type ExactPoint = {x: number, y: number};
type ExactDict = {[key in string]?: string};
type InexactPoint = {x: number, y: number, ...};
type InexactDict = {[key in string]?: string, ...};
```

## Assignability

Assignability refers to both assignment of a value to a variable, but also
passing a value to a function since this also assigns the value to a variable.

An exact object can be assigned to either another exact object or an inexact
object as long as the source and target types have the same fields.

Inexact objects cannot be assigned to exact object types.

Whether an object is exact/inexact and mutable/immutable must be taken into
account when determining assignability. See mutabliity.md for how mutability
affects assignability.

NOTE: Object literals are inferred as exact object types.

## Rest/Spread

Spreading an two exact objects will result in an exact object, but spreading
an inexact object with any other object type will result in an inexact object.

```ts
declare let exact1: {foo: number};
declare let exact2: {bar: string};
declare let inexact1: {foo: number, ...};
declare let inexact2: {bar: string, ...};

{...exact1, ...exact2} // {foo: number, bar: string}
{...exact1, ...inexact2} // {foo: number, bar: string, ...}
{...inexact1, ...exact2} // {foo: number, bar: string, ...}
{...inexact1, ...inexact2} // {foo: number, bar: string, ...}
```

Using the rest operator with an exact object will return an exact object and
an inexact object when using it with an inexact object.

```ts
declare let exact: {foo: number, bar: string};
declare let inexact: {foo: number, bar: string, ...};

let {foo, ...rest1} = exact; // rest1 is typed as {bar: string}
let {foo, ...rest2} = inexact; // rest2 is typed as {bar: string, ...}
```

## Object Spread Type

The intersection of two exact object types is empty unless the object types are
exactly the same. To combine exact object types that are different, the type-level spread operator must be used.

```ts
type Foo = { a: number; b: string };
type Bar = { b: number; c: boolean };

declare let foo: Foo;
declare let bar: Bar;

let foobar = { ...foo, ...bar }; // {a: number, b: number, c: boolean}
let barfoo = { ...bar, ...foo }; // {a: number, b: string, c: boolean}

type FooBar = {...Foo, ...Bar}; // {a: number, b: number, c: boolean}
type BarFoo = {...Bar, ...Foo}; // {a: number, b: string, c: boolean}
```

Spreading of objects containing optional fields results can result in the union
of both types for that field. If both fields are optional then the field in
the combined object type will also be optional.

```ts
type Foo = {x: number, y?: string};
type Bar = {y: number};
type Baz = {y?: number};

type FooBar = {...Foo, ...Bar}; // {x: number, y: number}
type BarFoo = {...Bar, ...Foo}; // {x: number, y: number | string}
type BazFoo = {...Baz, ...Foo}; // {x: number, y?: number | string}
```

Type-level spread should also be used to describe the type of objects that were
spread together. Using intersection types for this purpose is incorrect. This
is because:

- intersection of exact object types is unsatisfiable and is inferred as `never`
- intersection of inexact object types doesn't account for the order that objects
  are spread and instead combines overlapping fields using the intersection operator,

```ts
type Foo = {a: number, b: string, ...};
type Bar = {b: number, c: boolean, ...};

type FooAndBar = Foo & Bar; // {a: number, b: never, c: boolean};
```

`b` is `never` because `string & number` is not satisfiable.

## Generics

```ts
declare fn exactFn<T: {foo: number, bar: string}>(param: T) -> T;
declare fn inexactFn<T: {x: number, y: number, ...}>(param: T) -> T;

exactFn({foo: 5, bar: "hello"});
exactFn({foo: 5, bar: "hello", baz: true}); // error, extra field `baz` not allowed

inexactFn({foo: 5, bar: "hello"});
inexactFn({foo: 5, bar: "hello", baz: true}); // ok, type param constrait is inexact
```

## Interfaces and Classes

Both interfaces and classes are inexact object types. The reason for this is
that we'd like an interface type to unify with all classes that implement it
and all interfaces that extend it. Similarly, we'd like all classes to unify
with sub-classes that extend it.

## Object Methods

```ts
type ExactPoint = {x: number, y: number};
type ExactDict = {[key in string]?: string};
type InexactPoint = {x: number, y: number, ...};
type InexactDict = {[key in string]?: string, ...};

Object.keys(exactPoint); // ("x" | "y")[]
Object.keys(exactDict); // string[]
Object.keys(inexactPoint); // AnyKey[]
Object.keys(inexactDict); // AnyKey[]

Object.values(exactPoint); // number
Object.values(exactDict); // string
Object.values(inexactPoint); // unknown
Object.values(inexactDict); // unknown

Object.entries(exactPoint); // ["x" | "y", number][]
Object.entries(exactDict); // [string, string][]
Object.entries(inexactPoint); // [AnyKey, unknown][]
Object.entries(inexactDict); // [AnyKey, unknown][]
```

## Interop

Since TypeScript doesn't support exact object types, we need to be careful when
accepting object values from TypeScript or allowing TypeScript functions to
mutate an exact object type.

To minimize the possibility of there be extra properties in objects using exact
types, we have a few options:

- disallow passing objects to functions whose params mutable (we can check if
  the param is a `Readonly<>` type, but lots of code doesn't specify this when
  it could)
- create an inexact copy of an exact object before passing it to a TypeScript
  function
- add runtime check to development builds to check if an object from TypeScript
  has extra properties (inexact) or not (exact).

The type-level object spread operator doesn't exist in TypeScript, but really
should. https://github.com/microsoft/TypeScript/issues/10727 contains some
suggestions how one might implement a `Spread<>` utility type in TypeScript.

# Prior Art

- Flow: https://flow.org/en/docs/types/objects/#exact-and-inexact-object-types

# Implementation

- Add spread types
