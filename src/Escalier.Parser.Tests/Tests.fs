[<VerifyXunit.UsesVerify>]
module Tests

open Xunit
open VerifyTests
open VerifyXunit
open FParsec

open Escalier.Parser

let settings = VerifySettings()
settings.UseDirectory("snapshots")
settings.DisableDiff()

[<Fact>]
let ParseArithmetic () =
  let src = "0.1 + 2 * (3 - 4) / -5.6"
  let expr = run Parser.expr src
  let result = $"input: %s{src}\noutput: %A{expr}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseString () =
  let src = """msg = "Hello,\n\t\"world!\"" """
  let expr = run Parser.expr src
  let result = $"input: %s{src}\noutput: %A{expr}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseTemplateString () =
  let src = """msg = `foo ${`bar ${baz}`}`"""
  let expr = run Parser.expr src
  let result = $"input: %s{src}\noutput: %A{expr}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseFunctionCall () =
  let src = "add(x, y)"
  let expr = run Parser.expr src
  let result = $"input: %s{src}\noutput: %A{expr}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseFunctionCallExtraSpaces () =
  let src = "add( x , y )"
  let expr = run Parser.expr src
  let result = $"input: %s{src}\noutput: %A{expr}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseEmptyCall () =
  let src = "add()"
  let expr = run Parser.expr src
  let result = $"input: %s{src}\noutput: %A{expr}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseIndexer () =
  let src = "array[0]"
  let expr = run Parser.expr src
  let result = $"input: %s{src}\noutput: %A{expr}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseIndexerThenCall () =
  let src = "array[0]()"
  let expr = run Parser.expr src
  let result = $"input: %s{src}\noutput: %A{expr}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseCallThenIndexer () =
  let src = "foo()[0]"
  let expr = run Parser.expr src
  let result = $"input: %s{src}\noutput: %A{expr}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseFuncDef () =
  let src = "fn (x, y) { x }"
  let expr = run Parser.expr src
  let result = $"input: %s{src}\noutput: %A{expr}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseUnionType () =
  let src = "number | string | boolean"
  let expr = run Parser.typeAnn src
  let result = $"input: %s{src}\noutput: %A{expr}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseIntersectionType () =
  let src = "number & string & boolean"
  let expr = run Parser.typeAnn src
  let result = $"input: %s{src}\noutput: %A{expr}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseUnionAndIntersectionType () =
  let src = "A & B | C & D"
  let expr = run Parser.typeAnn src
  let result = $"input: %s{src}\noutput: %A{expr}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseArrayType () =
  let src = "number[][]"
  let expr = run Parser.typeAnn src
  let result = $"input: %s{src}\noutput: %A{expr}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseParenthesizedType () =
  let src = "(number | string)[]"
  let expr = run Parser.typeAnn src
  let result = $"input: %s{src}\noutput: %A{expr}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask
