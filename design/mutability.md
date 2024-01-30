# Mutability

Bindings are immutable by default. This means that once a binding is assigned
a value, it cannot be reassigned. A new binding with the same name can be
assigned a different value of the same type or even a different type.

```ts
let x = 5;
x = 10; // error - immutable bindings cannot be reassigned
```

```ts
let x = 5;
let x = "hello";
```

Immutable bindings to compound values (tuples, arrays, objects) do not allow
any part of the value to be modified either directly or by calling a method.

```ts
let arr = [1, 2, 3];
arr[0] = 10; // error - immutable bindings cannot mutate interior values
arr.push(4); // error - immutable bindings cannot call mutating methods
```

Mutable bindings can be defined using the `mut` keyword and allow both
reassignment and internal mutation of compound values.

```ts
let mut x = 5;
x = 10;

let mut arr = [1, 2, 3];
arr[0] = 10;
arr.push(4);
```

Mutable bindings can be assigned to immutable ones, but not the inverse.

```ts
type Point = {x: number, y: number};

let mut a: Point = {x: 5, y: 10};
let mut b = a;
let c: Point = {x: 0, y: 0};
let mut d = c; // error - immutable binding cannot be assigned to a mutable binding
```

This is also the case of function calls.

```ts
let printPoint = fn (point: Point) => {
    console.log(`(${point.x}, ${point.y})`)
};
let resetPoint = fn (mut point: Point) => {
    point.x = 0
    point.y = 0
};

let mut a = {x: 5, y: 10};
printPoint(a);
resetPoint(a);

let b = {x: 0, y: 0};
printPoint(b);
resetPoint(b); // error - immutable binding cannot be passed to a mutable param
```

Values can passed to functions as either immutable or mutable params. Only
bindings are mutable or not.

```ts
printPoint({ x: 5, y: 10 });
resetPoint({ x: 5, y: 10 });
```

## Variance

Mutability affects which types can be assigned to a bindings. By default,
immutable bindings allow sub-types to be assigned. This is safe because there's
no way to change an interior value to be the wrong type.

```ts
let a: number[] = [1, 2, 3];
let b: (number | string)[] = a;
```

When assigning a mutable binding to another mutable binding, both must be the
same type. This is also true for mutable params in functions.

```ts
let a: number[] = [1, 2, 3];
let b: string[] = a;
let c: (number | string)[] = a; // error - mutable binding assignments must be the same type
c.push("hello"); // whoops - `a` contains a string even though its type is `number[]`
```

## Destructuring & Pattern Matching

The `mut` keyword can also be on bindings in patterns. This allows some bindings
introduced to be mutable while others remain mutable.

```ts
type Line = {p0: Point, p1: Point};

let mut line = {p0: {x: 0, y: 0}, p1: {x: 5, y: 10}}
let {mut p0, p1: mut p2} = line
```

The expression on the right side of the initialization can containing bindings.
The compiler disallows mutable bindings from being assigned to immutable bindings
in these situations as well.

```ts
let p0: Point = {x: 0, y: 0}
let p1: Point = {x: 5, y: 10}

let {p0: mut q0, p1: q1} = {p0, p1} // error - immutable `p1`` cannot be assigned to mutable `q1`
```

This is also the case with function params that are destructured as well as
destructuring assignments that aren't introducing new bindings.

## Notes

It is possible mutate an immutable binding with the following code:

```ts
let mut p: Point = {x: 5, y: 10};
let q = p;
p.x = 0;
p.y = 0;
// `p` is bound to the same Point as `q` which is {x: 0, y: 0} now
```

This is still typesafe because `p` is still a `Point`.

## Questions

- Should an immutable binding be allowed to assigned to a mutable binding if
  its a primitive type?

  ```ts
  let a = 5
  let mut b: number = a
  b = 10
  ```
