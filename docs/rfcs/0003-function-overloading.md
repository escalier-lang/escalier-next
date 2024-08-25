# Summary

This RFC proposes syntax to allow easy definition of overloaded functions.

# Motivation

TypeScript provides a way to define overloaded functions, but it has a couple
of drawbacks. In particular, it requires the developer to check the types of
the incoming parameters before using them.

```ts
// good-overload.ts
type Vec2 = [number, number];

function add(a: number, b: number): number;
function add(a: Vec2, b: Vec2): Vec2;
function add(a: any, b: any) {
  if (typeof a === "number" && typeof b === "number") {
    return a + b;
  } else if (Array.isArray(a) && Array.isArray(b)) {
    return [a[0] + b[0], a[1] + b[1]];
  } else {
    throw new TypeError("Invalid args");
  }
}

const sum = add(5, 10);
const vec3 = add([5, 10], [1, 0]);
```

It's also possible to get the type checks wrong without TypeScript warning you
about this.

```ts
// bad-overload.ts
function add(a: number, b: number): number;
function add(a: Vec2, b: Vec2): Vec2;
function add(a: any, b: any) {
  if (typeof a === "number" && Array.isArray(b)) {
    return a + b[0];
  } else if (Array.isArray(a) && typeof b === "number") {
    return [a[0] + b, a[1] + 0];
  } else {
    throw new TypeError("Invalid args");
  }
}
```

# Explanation

Escalier will support function overloading by allowing multiple, full function
declarations.

```ts
// overload.esc
type Vec2 = [number, number];

fn add(a: number, b: number) -> number {
    return a + b;
}

fn add(a: Vec2, b: Vec2) -> Vec2 {
    return [a[0] + b[0], a[1] + b[1]];
}

const sum = add(5, 10);
const vec3 = add([5, 10], [1, 0]);
```

This syntax avoids having to manually type check the params. The compile will
automatically generate these for us.

Notes:

- Overloads for a function must be defined in one contiguous block. You cannot
  have other statements in between them. This is to avoid situations which
  wouldn't work for codegen, e.g. one overload capturing a variable in its
  closure that was the result of calling a different overload.
- Order matters. If there's any overlap in the domain of each overload, the
  first overload for which function call arguments satisfy the parameter types
  is the one that will be called.
- They must be declared using function declarations. Declaring overloads using
  variable declarations will not be allowed.
- Generic functions will not be allowed as overloads. This restriction may be
  relaxed in the future, but I'm worried it will complicate code generation and
  I'd like to avoid those complications for the time being.

## Subtyping

Functions with overloads will be inferred as the intersection of the type of
each of the overloads. Using the example above, this would make the inferred
type for `add` the following:

```ts
(add(a: number, b: number) -> number) & (add(a: Vec2, b: Vec2) -> Vec)
```

The intersection type is a subtype of each of the individual overloads.

## Usage Guidance

- Place overloads with more parameters before those with fewer.
- Place overloads with more specific parameter types before those with less
  specific parameter types.

## Codegen

It's possible that different overloads can have different numbers of parameters
and those parameters may have different names. We generating JavaScript for the
function we need to generate a single function from the different overloads.

To facilitate this, we will normalize parameter names generating new parameter
names like `__arg0__`, `__arg1__`, etc. and then renmaing the original parameter
names to the new ones during code generation.

## Interop

Type declarations for function overloads are exactly the same in TypeScript.
