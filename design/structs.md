# Structs

Structs are an alternative to JavaScript classes with a couple of notable
differences:

- allocation of structs cannot have side effects like constructors in classes
- methods are defined separately in `impl` blocks, more on that below

Here's what a basic struct for a point looks likes:

```rs
struct Point {
    x: number,
    y: number
}
```

New instances of a struct are created using the following syntax:

```rs
let point = Point { x: 5, y: 10 }
```

Structs are assignable to an object type with the same properties, but not the
other way arround.

```rs
let p = Point { x: 5, y: 10 }
let q: {x: number, y: number} = p
```

Structs can also be used as patterns when destructuring or pattern matching.

```rs
let point = Point {x: 5, y: 10}
let Point {x, y} = point
let {x, y} = point // also valid
```

Data members on structs can have the following modifiers:

- `private` can only be accessed by the struct's methods
- `readonly` cannot be modified even by a `mutable` binding

```rs
struct Foo {
    a: string
    private b: string
    readonly c: string
    private readonly baz: string
}

let mut foo = Foo { a: "a", b: "b", c: "c", d: "d" }

foo.a = "A"
foo.b = "B" // error - .b is private and cannot be accessed
foo.c = "C" // error - .c is readonly and cnnoat be modified
foo.d = "D" // error - .d is private and cannot be accessed
```

Data members can also be marked as optional:

```rs
struct Bar {
    msg?: string
}

let bar1 = Bar {}
let bar2 = Bar {msg: "hello"}
```

## Methods

Methods can defined on structs using one or more `impl` blocks. Instance methods
are passed a param called `self` which provides a binding the method's receiver.
It is equivalent to `this` in JavaScript with the main difference being that it's
an explicit binding instead of implicit.

```rs
impl Point {
    fn toString(self) {
        return `(${self.x}, ${self.y})`;
    }

    fn scale(mut self, factor) {
        self.x = factor * self.x;
        self.y = factor * self.y;
    }
}

let mut p = Point {x: 5, y: 10};
p.toString(); // "(5, 10)"
p.scale(2); // `p` is not `{x: 10, y: 20}`
```

Methods that mutate the receiver must mark the `self` as `mut`.

`impl` blocks can also contain getters and setters.

```rs
impl Point {
    get length(self) {
        return Math.sqrt(self.x * self.x + self.y * self.y);
    }
    set length(mut self, value) {
        let factor = value / self.length
        self.scale(factor)
    }
}
```

Getters can mutate the receiver. This can be useful for things like lazy
initialization.

```rs
struct Foo {
    private _msg: string | null
}

impl Foo {
    get msg(mut self) {
        if !self._msg_ {
            self._msg_ = ""
        }
        return self._msg_
    }
    set msg(mut self, value) {
        self._msg_ = value
    }
}
```

Getters can be called when destructuring structs.

```rs
let foo = Foo {};
let {msg} = foo; // calls the `bar` getter in the previous snippet
```

Methods can have all of the same modifiers as normal functions including: `async`,
`gen`, and `async gen`.

Static methods can be added to `impl` blocks by using the `static` keyword and
omitting the `self` param.

```rs
impl Foo {
    static fn new(msg) {
        let mut foo = Foo {}
        foo.msg = msg
        return foo
    }
}

let foo = Foo.new("hello")
```

## Generics

Structs can be generic as can methods on structs.

```rs
struct Node<T> {
    value: T
    left?: Node<T>
    right?: Node<T>
}

impl Node<T> {
    fn count(self, mut tally = 0) {
        if let {left} = self {
            tally += left.count()
        }
        if let {right} = self {
            tally += right.count()
        }
        return tally
    }
    fn map<U>(self, mapper: fn (x: T) -> U) -> Node<U> {
        return {
            value: mapper(self.value),
            left: self.left?.map(mapper),
            right: self.right?.map(mapper),
        }
    }
}
```

Any constraints on type params of the struct must be consistent across all
`impl` blocks for the `strct`.
