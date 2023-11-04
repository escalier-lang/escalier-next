module Tests

open Xunit
open Escalier.Parser
open Escalier.TypeChecker
open FsToolkit.ErrorHandling
open FParsec

type CompileError =
  | ParseError of ParserError
  | TypeError of Errors.TypeError

let infer src =
  result {
    let! ast =
      match Parser.expr src with
      | Success(value, _, _) -> Result.Ok(value)
      | Failure(s, parserError, unit) ->
        Result.mapError (CompileError.ParseError) (Result.Error(parserError))

    let env =
      { Infer.Env.values = Map([])
        Infer.Env.types = Map([]) }

    let! t = Result.mapError (CompileError.TypeError) (Infer.infer_expr env ast)
    return t
  }

[<Fact>]
let InfersLiterals () =
  result {
    let! num = infer "5"
    Assert.Equal("5", num.ToString())

    let! str = infer "\"hello\""
    Assert.Equal("\"hello\"", str.ToString())

    let! bool = infer "true"
    Assert.Equal("True", bool.ToString())

    let! bool' = infer "false"
    Assert.Equal("False", bool'.ToString())
  }

[<Fact>]
let InferIfElse () =
  let result =
    result {
      let! t = infer "if (true) { 5 } else { \"hello\" }"
      Assert.Equal("5 | \"hello\"", t.ToString())
    }

  Assert.False(Result.isError result)
