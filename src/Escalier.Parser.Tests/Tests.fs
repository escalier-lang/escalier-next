[<VerifyXunit.UsesVerify>]
module Tests

open Xunit
open VerifyTests
open VerifyXunit
open FParsec

open Escalier.Parser

let settings = new VerifySettings()
settings.UseDirectory("snapshots")

[<Fact>]
let ``My test`` () = Assert.True(true)

[<Fact>]
let ParseArithmetic () =
  let src = "0.1 + 2 * (3 - 4) / -5.6"
  let expr = run ExprParser.expr src
  let result = sprintf "input: %s\noutput: %A" src expr

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseString () =
  let src = """msg = "Hello,\n\t\"world!\"" """
  let expr = run ExprParser.expr src
  let result = sprintf "input: %s\noutput: %A" src expr

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseTemplateString () =
  let src = """msg = `foo ${`bar ${baz}`}`"""
  let expr = run ExprParser.expr src
  let result = sprintf "input: %s\noutput: %A" src expr

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseFunctionCall () =
  let src = "add(x, y)"
  let expr = run ExprParser.expr src
  let result = sprintf "input: %s\noutput: %A" src expr

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseFunctionCallExtraSpaces () =
  let src = "add( x , y )"
  let expr = run ExprParser.expr src
  let result = sprintf "input: %s\noutput: %A" src expr

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseEmptyCall () =
  let src = "add()"
  let expr = run ExprParser.expr src
  let result = sprintf "input: %s\noutput: %A" src expr

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseIndexer () =
  let src = "array[0]"
  let expr = run ExprParser.expr src
  let result = sprintf "input: %s\noutput: %A" src expr

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseIndexerThenCall () =
  let src = "array[0]()"
  let expr = run ExprParser.expr src
  let result = sprintf "input: %s\noutput: %A" src expr

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

[<Fact>]
let ParseCallThenIndexer () =
  let src = "foo()[0]"
  let expr = run ExprParser.expr src
  let result = sprintf "input: %s\noutput: %A" src expr

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask
