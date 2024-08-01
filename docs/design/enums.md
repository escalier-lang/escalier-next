# Enums

Enums in Escalier have the following properties:

- can have associated data in form of a single value or an object
- each variant gets a unique opaque id
- the enum's type is a union of each variant type

## Declaring

```ts
enum MyEnum {
    Foo,
    Bar(string),
    Baz {msg: string, read: boolean},
}
```

## Creating

Enum instances can be created in the following way:

```ts
let foo = MyEnum.Foo;
let bar = MyEnum.Bar("none");
let baz = MyEnum.Baz {msg: "hello", read: true};
```

## Consuming

Values can be extract via pattern matching as well as `if-let` and `let-else`.

```ts
declare let value: MyEnum;

fn getValue(x: MyEnum) {
    return match x {
        MyEnum.Foo => "",
        MyEnum.Bar(value) => value,
        MyEnum.Baz {msg} => msg,
    };
}

let msg = if let MyEnum.Baz {msg} = value { msg } else { "" }
let MyEnum.Baz {msg} = value else { "" }
```

## Combining

```ts
enum MyEnum {
    Foo,
    Bar(string),
    Baz {msg: string, read: boolean},
}

enum MyOtherEnum {
    Qux(string),
}

type MyCombinedEnum = MyEnum | MyOtherEnum;
```

Since each enum variant is given a unique identifier, it's safe to combine
enums using a union type.

## Interop

TypeScript can create and consume Escalier enum values in the following way:

```ts
import {MyEnum} from "./my_enum.esc";

let foo: MyEnum = {__TAG__: MyEnum.Foo};
let bar: MyEnum = {__TAG__: MyEnum.Bar, __VALUE__: "none"};
let baz: MyEnum = {__TAG__: MyEnum.Baz, msg: "hello", read: true};

function getValue(x: MyEnum) {
    switch x.__TAG__ {
        MyEnum.Foo: return ""
        MyEnum.Bar: return x.__VALUE__
        MyEnum.Baz: return x.msg
    }
}
```

Writing these objects out by hand is a bit tedious and requires knowledge about
the internal layout of the enum variants. To avoid this, we could have helper
functions that could be imported from `@escalier/interop` that:

- creating variant values
- checking which variant a value is

Escalier will also be able to consume tagged unions from TypeScript as enums:

```ts
// shape.ts
export type Shape =
  | {kind: "Rect", width: number, height: number}
  | {kind: "Circle", radius: number};

// draw_shape.esc
import {Shape} from "./shape";

fn drawShape(ctx: CanvasRenderingContext2D, shape: Shape) {
    match shape {
        Shape.Rect {width, height} {
            ctx.fillRect(0, 0, width, height);
        }
        Shape.Circle {radius} {
            ctx.fillEllipse(0, 0, radius, radius);
        }
    };
}
```
