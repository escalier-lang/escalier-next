[<VerifyXunit.UsesVerify>]
module Tests

open Xunit
open VerifyTests
open VerifyXunit
open FParsec

open Escalier.Parser

let lit = run (Parser.lit .>> eof)
let expr = run (Parser.expr .>> eof)
let typeAnn = run (Parser.typeAnn .>> eof)

let settings = VerifySettings()
settings.UseDirectory("snapshots")
settings.DisableDiff()

[<Fact>]
let ParseArithmetic () =
  let src = "0.1 + 2 * (3 - 4) / -5.6"
  let expr = expr src
  let result = $"input: %s{src}\noutput: %A{expr}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseString () =
  let src = """msg = "Hello,\n\t\"world!\"" """
  let expr = expr src
  let result = $"input: %s{src}\noutput: %A{expr}"

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
  let src = """msg = `foo ${`bar ${baz}`}`"""
  let expr = expr src
  let result = $"input: %s{src}\noutput: %A{expr}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseFunctionCall () =
  let src = "add(x, y)"
  let expr = expr src
  let result = $"input: %s{src}\noutput: %A{expr}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseFunctionCallExtraSpaces () =
  let src = "add( x , y )"
  let expr = expr src
  let result = $"input: %s{src}\noutput: %A{expr}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseEmptyCall () =
  let src = "add()"
  let expr = expr src
  let result = $"input: %s{src}\noutput: %A{expr}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseIndexer () =
  let src = "array[0]"
  let expr = expr src
  let result = $"input: %s{src}\noutput: %A{expr}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseMultipleIndexers () =
  let src = "array[0][1]"
  let expr = expr src
  let result = $"input: %s{src}\noutput: %A{expr}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseIndexerThenCall () =
  let src = "array[0]()"
  let expr = expr src
  let result = $"input: %s{src}\noutput: %A{expr}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseCallThenIndexer () =
  let src = "foo()[0]"
  let expr = expr src
  let result = $"input: %s{src}\noutput: %A{expr}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseObjProperty () =
  let src = "obj.a.b"
  let expr = expr src
  let result = $"input: %s{src}\noutput: %A{expr}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseObjPropWithOptChain () =
  let src = "obj?.a?.b"
  let expr = expr src
  let result = $"input: %s{src}\noutput: %A{expr}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseFuncDef () =
  let src = "fn (x, y) { x }"
  let expr = expr src
  let result = $"input: %s{src}\noutput: %A{expr}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseFuncDefWithTypes () =
  let src = "fn (x: number) -> number throws \"RangeError\" { x }"
  let expr = expr src
  let result = $"input: %s{src}\noutput: %A{expr}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseFuncDefWithTypeParams () =
  let src = "fn <T: Foo = Bar>(x: T) -> T { x }"
  let expr = expr src
  let result = $"input: %s{src}\noutput: %A{expr}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseTuple () =
  let src = "[1, 2, 3]"
  let expr = expr src
  let result = $"input: %s{src}\noutput: %A{expr}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseUnionType () =
  let src = "number | string | boolean"
  let expr = typeAnn src
  let result = $"input: %s{src}\noutput: %A{expr}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseUnionTypeWithLiterals () =
  let src = "5 | \"hello\""
  let expr = typeAnn src
  let result = $"input: %s{src}\noutput: %A{expr}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseIntersectionType () =
  let src = "number & string & boolean"
  let expr = typeAnn src
  let result = $"input: %s{src}\noutput: %A{expr}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseUnionAndIntersectionType () =
  let src = "A & B | C & D"
  let expr = typeAnn src
  let result = $"input: %s{src}\noutput: %A{expr}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseArrayType () =
  let src = "number[][]"
  let expr = typeAnn src
  let result = $"input: %s{src}\noutput: %A{expr}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseParenthesizedType () =
  let src = "(number | string)[]"
  let expr = typeAnn src
  let result = $"input: %s{src}\noutput: %A{expr}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseFunctionType () =
  let src = "fn <T: Foo = Bar>(x: T) -> T throws \"RangeError\""
  let expr = typeAnn src
  let result = $"input: %s{src}\noutput: %A{expr}"

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
