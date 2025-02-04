# Summary

This RFC proposes reworking existing Escalier enums to:
- add support for extensible enums
- use the TC39 "Extractors" proposal to implement enums

# Motivation

TypeScript enums are not very powerful. They only support number or string values. There is no way
to associate data with an enum variant and there's no way to extend them.

Escalier implemented a more powerful enum system that allows for associated data with each variant, but
the type checking uses complicated intersection types that are difficult to work with. It also doesn't
support extensible enums.

This RFC proposes a new way to define enums based on the TC39 "Extractors" proposal:
https://github.com/tc39/proposal-extractors. This proposal is currently at stage 2. This proposal mentions that
that extractors could be used to add support for enums in ECMAScript in the future. 

# Explanation

## Syntax

```ts
enum MyEnum {
    Foo,
    Bar(value: string),
    Baz(message: {content: string, read: boolean}, count: number),
}

// variants without associated data can be constructed with just the variant name
val foo = MyEnum.Foo;
// variants with associated data can be constructed by calling the variant name as a function
val bar = MyEnum.Bar("hello");
// variants can have multiple bits of data associated with them
val baz = MyEnum.Baz({content: "you have mail", read: true}, 1);

val strs = ["hello", "world"];
val bars = strs.map(MyEnum.Bar); // variants can be used a values in higher order functions

// pattern matching
val value = match x {
    MyEnum.Foo => "",
    MyEnum.Bar(value) => value,
    MyEnum.Baz({content}, _) => content,
};

// if-val
val msg = if val MyEnum.Baz {msg} = value { 
    msg
} else {
    ""
}

// val-else
val MyEnum.Baz {msg} = value else { "" }

// extensible enums
enum MyOtherEnum extends MyEnum {
    Qux(value: string),
}
```

NOTE: This removes the struct-like syntax for defining object variants that's available in the current
implementation. Whenever structs are added to Escalier, enums will be updated to use them.

## Implementation

Each of an enum's variants will be implemented as their own class each with their own `[Symbol.customMatcher]()`
method. The enum itself will be an object containing each of the variants. The enum's type will be a
union of each variant's type.

The code below shows how the `MyEnum` and `MyOtherEnum` enums would be implemented in TypeScript.  The Escalier
compiler will output equivalent .js and .d.ts code. Because we're able to fully represent the enum in TypeScript,
we won't need to do anything special when importing enums from packages that were compiled from Escalier.

```ts
namespace MyEnum {
    export class Foo {
        [Symbol.customMatcher]() {
            return [];
        }
    }
    export class Bar {
        #value: string;
        constructor(value: string) {
            this.#value = value;
        }
        [Symbol.customMatcher]() {
            return [this.#value];
        }
    }
    export class Baz {
        #message: {content: string, read: boolean};
        #count: number;
        constructor(message: {content: string, read: boolean}, count: number) {
            this.#message = message;
            this.#count = count;
        }
        [Symbol.customMatcher]() {
            return [this.#message, this.#count];
        }
    }
}

type MyEnum = MyEnum.Foo | MyEnum.Bar | MyEnum.Baz;

namespace MyOtherEnum {
    export const Foo = MyEnum.Foo;
    export type Foo = MyEnum.Foo;
    export const Bar = MyEnum.Bar;
    export type Bar = MyEnum.Bar;
    export const Baz = MyEnum.Baz;
    export type Baz = MyEnum.Baz;
    export class Qux {
        #value: string;
        constructor(value: string) {
            this.#value = value;
        }
        [Symbol.customMatcher]() {
            return [this.#value];
        }
    }
};

type MyOtherEnum = MyOtherEnum.Foo | MyOtherEnum.Bar | MyOtherEnum.Baz | MyOtherEnum.Qux;
```

Pattern matching will need to check the instance of each variant before destructuring it.  The following Escalier code:
```ts
val value = match x {
    MyEnum.Foo => "",
    MyEnum.Bar(value) => value,
    MyEnum.Baz({content}, _) => content,
};
```
will be transformed into the following JavaScript code:
```ts
let temp0;
if (x instanceof MyEnum.Foo) {
    temp0 = "";
} else {
    let temp1;
    if (x instanceof MyEnum.Bar) {
        const [value] = x[Symbol.customMatcher]();
        temp1 = value;
    } else {
        let temp2;
        if (x instanceof MyEnum.Baz) {
            const [message, _] = x[Symbol.customMatcher]();
            const {content} = message;
            temp2 = content;
        }
        temp1 = temp2;
    }
    temp0 = temp1;
}
const value = temp0;
```
