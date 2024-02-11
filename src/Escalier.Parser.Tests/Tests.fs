[<VerifyXunit.UsesVerify>]
module Tests

open Xunit
open VerifyTests
open VerifyXunit
open FParsec

open Escalier.Parser

let lit = run (Parser.lit .>> eof)
let typeAnn = run (Parser.typeAnn .>> eof)

let settings = VerifySettings()
settings.UseDirectory("snapshots")
settings.DisableDiff()

[<Fact>]
let ParseArithmetic () =
  let src = "0.1 + 2 * (3 - 4) / -5.6"
  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseString () =
  let src = """let msg = "Hello,\n\t\"world!\"" """
  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseOtherLiterals () =
  let src =
    """
    let a = undefined
    let b = null
    """

  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseTemplateString () =
  let src = """let msg = `foo ${`bar ${baz}`}`"""
  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseFunctionCall () =
  let src = "add(x, y)"
  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseFunctionCallExtraSpaces () =
  let src = "add( x , y )"
  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseEmptyCall () =
  let src = "add()"
  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseIndexer () =
  let src = "array[0]"
  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseMultipleIndexers () =
  let src = "array[0][1]"
  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseIndexerThenCall () =
  let src = "array[0]()"
  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseCallThenIndexer () =
  let src = "foo()[0]"
  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseObjProperty () =
  let src = "obj.a.b"
  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseObjPropWithOptChain () =
  let src = "obj?.a?.b"
  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseFuncDef () =
  let src = "fn (x, y) { x }"
  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseFuncDefWithTypes () =
  let src = "fn (x: number) -> number throws \"RangeError\" { x }"
  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseFuncDefWithTypeParams () =
  let src = "fn <T: Foo = Bar>(x: T) -> T { x }"
  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseTuple () =
  let src = "[1, 2, 3]"
  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseUnionType () =
  let src = "number | string | boolean"
  let ast = typeAnn src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseUnionTypeWithLiterals () =
  let src = "5 | \"hello\""
  let ast = typeAnn src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseIntersectionType () =
  let src = "number & string & boolean"
  let ast = typeAnn src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseUnionAndIntersectionType () =
  let src = "A & B | C & D"
  let ast = typeAnn src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseArrayType () =
  let src = "number[][]"
  let ast = typeAnn src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseParenthesizedType () =
  let src = "(number | string)[]"
  let ast = typeAnn src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseFunctionType () =
  let src = "fn <T: Foo = Bar>(x: T) -> T throws \"RangeError\""
  let ast = typeAnn src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseObjLitAndObjPat () =
  let src =
    """
    type Point = {x: number, y: number}
    let {x, y}: Point = {x: 5, y: 10}
    let p: Point = {x, y}
    let foo = fn ({x, y}: Point) => x + y
    """

  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseObjRestSpread () =
  let src =
    """
    let obj = {a: 5, b: "hello", c: true}
    let {a, ...rest} = obj
  """

  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseOptionalProps () =
  let src =
    """
    type Obj = {a?: {b?: {c?: number}}}
    """

  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseOptionalParams () =
  let src =
    """
    let foo = fn(a?: number, b?: string) => a
    """

  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseArrowIdentifier () =
  let src = "let fst = fn (x, y) => x"
  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseImports () =
  let src =
    """
    import "./math" {add, sub as subtract}
    import "~/net" as network
    import "path"
    """

  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseConditionalTypeAnn () =
  let src =
    """
    type Exclude<T, U> = if T: U { never } else { T }
    """

  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseChainedConditionalTypeAnn () =
  let src =
    """
    type Foo<T> = if T: number { "number" } else if T: string { "string" } else { "other" }
    """

  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseMappedTypes () =
  let src = """type Point = {[P]: number for P in "x" | "y"}"""
  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseIndexAccessTypes () =
  let src = "type Foo = Bar[\"baz\"]"
  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseAsyncFunc () =
  let src =
    """
    let bar = async fn () {
      let x = await foo()
      return x + 10
    }
    """

  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseThrowAndTryCatch () =
  let src =
    """
    let foo = fn (x) {
      if x < 0 {
        throw "x must be positive"
      }
      return x
    }
    let bar = fn (x) {
      let result = try {
        foo(x)
      } catch {
        | _ => 0
      }
      return result
    }
    """

  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParsePatternMatching () =
  let src =
    """
    let foo = fn (x) =>
      match x {
        | 0 => "none"
        | 1 => "one"
        | n if n < 0 => "negative"
        | _ => "other"
      }
    """

  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseComplexPatternMatching () =
  let src =
    """
    match node {
      | {x, y: b} => 1
      | {x is number, y: b is number} => 2
      | {x: _, y: _ is number} => 3
      | {type: "circle", center: {x, y}, radius} => 4
      | {x = 0, y: b = 0} => 5
      | {x is number = 0, y: b is number = 0} => 6
    }
    """

  // TODO: move the default value from the key-value pattern to the ident pattern
  // so that we can do stuff like
  // | {x: a = 0 is number, y: b = 0 is number} => 6
  // when matching something like {x?: number, y?: number}

  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask


[<Fact>]
let ParseCallableType () =
  let src =
    """
    type Callable = {
      new fn () -> symbol,
      fn () -> symbol,
    }
    """

  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParsePropKeysInObjectType () =
  let src =
    """
    type Array = {
      5: string,
      "hello": number,
      [Symbol.iterator]: fn () -> Iterator<T>,
    }
    """

  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseForLoop () =
  let src =
    """
    for x in [1, 2, 3] {
      print(x)
    }
    """

  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseDeclare () =
  let src =
    """
    declare let foo: number
    declare let bar: string
    """

  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseRange () =
  let src =
    """
    type DieRoll = 1..6
    let range = 0..10
    """

  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseRangeIteratorType () =
  let src =
    """
      type RangeIterator<Min: number, Max: number> = {
        next: fn () -> { done: boolean, value: Min..Max }
      }
    """

  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseTemplateLiteralType () =
  let src =
    """
      type TemplateLiteral = `foo${number}`
    """

  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseInferType () =
  let src =
    "type ReturnType<T> = if T: fn (...args: _) -> infer R { R } else { never }"

  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseRecordTuple () =
  let src =
    """
    let p0 = #[5, 10]
    let p1 = #{x: 5, y: 10}
    let line = #{p0, p1}
    """

  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseMutableBindings () =
  let src =
    """
    type Point = {x: number, y: number}
    type Line = {p0: Point, p1: Point}
    declare let line: Line
    
    let {mut p0, p1: mut q} = line
    let mut r = q
    """

  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseMutableParams () =
  let src =
    """
    let update = fn (mut array: number[]) {
      for i in 0..array.length {
        array[i] = array[i] + 1
      }
    }
    """

  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseTypeof () =
  let src =
    """
    let iterator: typeof Symbol.iterator = Symbol.iterator
    """

  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseStruct () =
  let src =
    """
    struct Point {
      x: number,
      y: number,
    }
    """

  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseGenericStruct () =
  let src =
    """
    struct Point<T> {
      x: T,
      y: T,
    }
    """

  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseBasicImpl () =
  let src =
    """
    impl Foo {
      fn bar(self) {
        return self.x
      }
      fn baz(mut self, x: number) {
        self.x = x
      }
    }
    """

  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask
  
[<Fact>]
let ParseImplWithStaticMethods () =
  let src =
    """
    impl Point {
      fn new(x, y) {
        return Point { x, y }
      }
      fn default() {
        return Point { x: 0, y: 0 }
      }
    }
    """

  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseGenericImpl () =
  let src =
    """
    impl Foo<T> {
      fn bar(self) {
        return self.x
      }
      fn baz(mut self, x: T) {
        self.x = x
      }
    }
    """

  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseGetterSetterImpl () =
  let src =
    """
    impl Foo {
      get bar(self) {
        return self.x
      }
      set bar(mut self, x: number) {
        self.x = x
      }
    }
    """

  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask


[<Fact>]
let ParseStructExprs () =
  let src =
    """
    let foo = Foo { a: 5, b: "hello" }
    let bar = Bar<number> { a: 5, b: "hello" }
    """

  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseBasicStructPattern () =
  let src =
    """
    let Point {x, y} = point
    """

  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseGenericStructPattern () =
  let src =
    """
    let Point<number> {x, y} = point
    """

  let ast = Parser.parseScript src
  let result = $"input: %s{src}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask
