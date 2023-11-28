[<VerifyXunit.UsesVerify>]
module Tests

open VerifyTests
open VerifyXunit
open Xunit

open Escalier.Interop.Parser


let settings = VerifySettings()
settings.UseDirectory("snapshots")
settings.DisableDiff()

[<Fact>]
let ParseBasicVarDecls () =
  let input =
    """
    declare var a: A;
    declare var b: B.C;
    declare var c: number;
    declare var d: string | boolean;
    """

  let ast = parseModule input
  let result = $"input: %s{input}\noutput: %A{ast}"

  Verifier.Verify(result, settings).ToTask() |> Async.AwaitTask
