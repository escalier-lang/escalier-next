# Summary

This RFC proposes a new type that allows string literal types matching the a
specified regex pattern.

# Motiviation

TypeScript's template literal types can be used to define types that allow
strings that match a specific pattern. The following example shows a custom
type that matches CSS hex colors:

```ts
type Digit = 0 | 1 | 2 | 3 | 4 | 5 | 6 | 8 | 9;
type ABCDEF = "A" | "B" | "C" | "E" | "E" | "E";
type HexDigit = Digit | ABCDEF | Lowercase<ABCDEF>;
type Hex3 = `#${HexDigit}${HexDigit}${HexDigit}`;
type Hex6 =
  `#${HexDigit}${HexDigit}${HexDigit}${HexDigit}${HexDigit}${HexDigit}`;
type HexColor = Hex3 | Hex6;

let red: HexColor = "#F00";
let cyan: HexColor = "#00FFFF";
```

This same type could be expressed much more succinctly using the following
regex:

```ts
let hexColorRegex = /^#([0-9a-f]{3}|[0-9a-f]{6})$/i;
```

# Explanation

Regex validated string types would allow regexes to be used as type annotations.
Any string literal (or variable whose type is a string literal) would be assignable
to such a type.

```ts
type HexColor = /^#([0-9a-f]{3}|[0-9a-f]{6})$/i;

let red: HexColor = "#F00";
let cyan: HexColor = "#00FFFF";

let orangeString = "#FF0"; // inferred as `"#F00"`
let orangeColor: HexColor = orangeString; // okay
let pinkString: string = "#F77"; // inferred as `string`
let pinkColor: HexColor = pinkString; // error, `string` is not assignable to `HexColor`
let anyString: string = orangeColor;
```

## Subtyping

Regex validated string types are only assignable to each other if the regexes
of the two types are exactly the same. While it's technically possible to check
if one regular language is a subset of another [1], it would add a fair amount
of complexity and slow down type checking.

[1]: https://math.stackexchange.com/questions/283838/is-one-regular-language-subset-of-another

## Regex Interpolation

Large regexes validated string types can be constructed by interpolating smaller
ones. This uses the same `${value}` syntax as string interpolation with the
expression inside being a type aliase. The type alias must point to a regex
validated string type.

```ts
type HexDigit = /^[0-9a-f]$/i;
type HexColor = /^#(${HexDigit}{3}|${HexDigit}{6})$/;
```

Unbounded regexes will be interpolated approprately.

```ts
type Word = /\w+/;
type DelimitedWord = /^\(${Word}\)$/; // equivalent to `/^\(.*\w+.*)$/`

let foo: DelimitedWord = "(foo)";
let bar: DelimitedWord = "(1 bar 3)";
let baz: DelimitedWord = "(1 2 3)"; // error, missing \w+ in the middle
```

## Usage Guidance

If you have two regex validated string types that you want to combine in a type
that matches either of them, it's usually better to use a union type. This is
because the two original types will be recognized as subtypes.

```ts
type Letter = /^[a-z]$/i;
type Number = /^[0-9]$/i;

type Regex = /^(${Letter}|${Number})$/;
type Union = Letter | Number;

let letter: Letter = "A";
let number: Number = "1";

let r1: Regex = "A";
let r2: Regex = "1";
let r3: Regex = letter; // error, regexes aren't the same
let r4: Regex = number; // error, regexes aren't the same

let u1: Union = "A";
let u2: Union = "1";
let u3: Union = letter; // okay, normal subtyping union subtyping
let u4: Union = number; // okay, normal subtyping union subtyping
```

## Interop

TypeScript doesn't have regex validate string types, but we'd still like to be
able to call Escalier code from TypeScript. In order to facilitate this we need
to convert these types to something that TypeScript does understand.

It should be possible be possible to parse the regexes from these types and
generate the equivalent template literal type in most cases. Some patterns
will be harder than others but most should be doable:

- negated character classes, e.g. `[^abc]` - conditional type
- `x*` and `x+` - recursive coditional type
- `x?` - conditional type
- `x{n}` - repeat the type multiple `n` times

Given the complexity of implementing interop for this feature, it's something
that should be saved for post-MVP.

# Previous Art

- [Template Literal Types](https://www.typescriptlang.org/docs/handbook/2/template-literal-types.html)
- [#6579: Suggestion: Regex-validated string type](https://github.com/microsoft/TypeScript/issues/6579)
