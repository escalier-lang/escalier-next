module Tests

open Xunit
open Escalier.Parser
open Escalier.TypeChecker
open Escalier.Data.Type
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
        Infer.Env.schemes = Map([])
        Infer.Env.isAsync = false
        Infer.Env.nonGeneric = Set([]) }

    let! t = Result.mapError (CompileError.TypeError) (Infer.infer_expr env ast)
    return t
  }

let infer_script src =
  result {
    let! script =
      match Parser.script src with
      | Success(value, _, _) -> Result.Ok(value)
      | Failure(s, parserError, unit) ->
        Result.mapError (CompileError.ParseError) (Result.Error(parserError))

    let env =
      { Infer.Env.values = Map([])
        Infer.Env.schemes = Map([])
        Infer.Env.isAsync = false
        Infer.Env.nonGeneric = Set([]) }

    let! env =
      Result.mapError (CompileError.TypeError) (Infer.infer_script env script)

    return env
  }

let infer_with_env src env =
  result {
    let! ast =
      match Parser.expr src with
      | Success(value, _, _) -> Result.Ok(value)
      | Failure(s, parserError, unit) ->
        Result.mapError (CompileError.ParseError) (Result.Error(parserError))

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
    Assert.Equal("true", bool.ToString())

    let! bool' = infer "false"
    Assert.Equal("false", bool'.ToString())
  }

[<Fact>]
let InferBinaryOperators () =
  let result =
    result {
      let! sum = infer "5 + 10"
      Assert.Equal("number", sum.ToString())

      let! lt = infer "5 < 10"
      Assert.Equal("boolean", lt.ToString())

      let! eq = infer "\"hello\" == 5"
      Assert.Equal("boolean", eq.ToString())

      let! b = infer "true || false"
      Assert.Equal("boolean", b.ToString())
    }

  Assert.False(Result.isError result)

[<Fact>]
let InferIfElse () =
  let result =
    result {
      let! t = infer "if (true) { let x = 5\nx } else { \"hello\" }"
      Assert.Equal("5 | \"hello\"", t.ToString())
    }

  Assert.False(Result.isError result)

[<Fact>]
let InferIfElseChaining () =
  let result =
    result {
      let src =
        """
      let foo = if (true) {
        5
      } else if (false) {
        "hello"
      } else {
        true
      }
      """

      let! env = infer_script src
      let (foo, _) = Map.find "foo" env.values
      Assert.Equal("5 | \"hello\" | true", foo.ToString())
    }

  Assert.False(Result.isError result)

[<Fact>]
let InferIdentifier () =
  let t: Type =
    { Type.kind = TypeKind.Primitive(Primitive.Number)
      provenance = None }

  let env =
    { Infer.Env.values = Map([ ("foo", (t, false)) ])
      Infer.Env.schemes = Map([])
      Infer.Env.isAsync = false
      Infer.Env.nonGeneric = Set([]) }

  let result =
    result {
      let! t = infer_with_env "foo" env
      Assert.Equal("number", t.ToString())
    }

  Assert.False(Result.isError result)

[<Fact>]
let InferLetStatements () =
  let result =
    result {
      let! env = infer_script "let foo = 5\nlet bar =\"hello\""
      let (foo, _) = Map.find "foo" env.values
      let (bar, _) = Map.find "bar" env.values
      Assert.Equal("5", foo.ToString())
      Assert.Equal("\"hello\"", bar.ToString())
    }

  Assert.False(Result.isError result)

[<Fact>]
let InferBinOpsOnPrimitives () =
  let result =
    result {
      let src =
        """
          let x = 5
          let y = 10
          let sum = x + y
          """

      let! env = infer_script src
      let (sum, _) = Map.find "sum" env.values
      Assert.Equal("number", sum.ToString())
    }

  Assert.False(Result.isError result)

[<Fact>]
let InferFuncParams () =
  let result =
    result {
      let src =
        """
          let add = fn (x, y) {
            return x + y
          }
          """

      let! env = infer_script src
      let (sum, _) = Map.find "add" env.values
      Assert.Equal("fn (x: number, y: number) -> number", sum.ToString())
    }

  Assert.False(Result.isError result)

[<Fact>]
let InferFuncParamsWithTypeAnns () =
  let result =
    result {
      let src =
        """
          let add = fn (x: number, y: number) -> number {
            return x + y
          }
          """

      let! env = infer_script src
      let (sum, _) = Map.find "add" env.values
      Assert.Equal("fn (x: number, y: number) -> number", sum.ToString())
    }

  Assert.False(Result.isError result)

[<Fact>]
let InferFuncWithMultipleReturns () =
  let result =
    result {
      let src =
        """
          let foo = fn (x: number, y: string) {
            if (x > 0) {
              return x
            }
            return y
          }
          """

      let! env = infer_script src
      let (foo, _) = Map.find "foo" env.values

      Assert.Equal(
        "fn (x: number, y: string) -> string | number",
        foo.ToString()
      )
    }

  Assert.False(Result.isError result)

[<Fact>]
let InferFuncGenericFunc () =
  let result =
    result {
      let src =
        """
          let foo = fn (x) {
            return x
          }
          """

      let! env = infer_script src
      let (sum, _) = Map.find "foo" env.values
      Assert.Equal("fn <A>(x: A) -> A", sum.ToString())
    }

  Assert.False(Result.isError result)

[<Fact>]
let InferTypeDecls () =
  let result =
    result {
      let src =
        """
          type A = number
          type B = [string, boolean]
          type C = 5 | "hello"
          type D = fn (x: number) -> number
          """

      let! env = infer_script src

      let a = Map.find "A" env.schemes
      Assert.Equal("number", a.type_.ToString())
      let b = Map.find "B" env.schemes
      Assert.Equal("[string, boolean]", b.type_.ToString())
      let c = Map.find "C" env.schemes
      Assert.Equal("5 | \"hello\"", c.type_.ToString())
      let d = Map.find "D" env.schemes
      Assert.Equal("fn (x: number) -> number", d.type_.ToString())
    }

  Assert.False(Result.isError result)
