[<VerifyXunit.UsesVerify>]
module ParserTests

open Xunit
open VerifyTests
open VerifyXunit
open FParsec

let lit = run Escalier.HindleyMilner.Parser.lit
let expr = run Escalier.HindleyMilner.Parser.expr
let pattern = run Escalier.HindleyMilner.Parser.pattern
let stmt = run Escalier.HindleyMilner.Parser.stmt
let typeAnn = run Escalier.HindleyMilner.Parser.typeAnn

let script = run (many Escalier.HindleyMilner.Parser.stmt)


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
