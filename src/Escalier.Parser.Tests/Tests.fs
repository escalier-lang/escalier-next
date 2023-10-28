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
let ``My test 2`` () =
  let src = "1 + 2 * -abc"
  let expr = run ExprParser.expr src
  let result = sprintf "input: %s\noutput: %A" src expr

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask

do ()
