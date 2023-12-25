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
      } catch e {
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
