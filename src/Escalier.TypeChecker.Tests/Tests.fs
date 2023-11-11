module Tests

open Xunit
open Escalier.Parser
open Escalier.TypeChecker
open Escalier.TypeChecker.Env
open Escalier.Data.Type
open FsToolkit.ErrorHandling
open FParsec

type Assert with

  static member inline Value(env: Env, name: string, expected: string) =
    let (t, _) = Map.find name env.values
    Assert.Equal(expected, t.ToString())

  static member inline Type(env: Env, name: string, expected: string) =
    let scheme = Map.find name env.schemes
    Assert.Equal(expected, scheme.type_.ToString())

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
      { Env.values = Map([])
        Env.schemes = Map([])
        Env.isAsync = false
        Env.nonGeneric = Set([]) }

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
      { Env.values = Map([])
        Env.schemes = Map([])
        Env.isAsync = false
        Env.nonGeneric = Set([]) }

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

      Assert.Value(env, "foo", "5 | \"hello\" | true")
    }

  Assert.False(Result.isError result)

[<Fact>]
let InferIdentifier () =
  let t: Type =
    { Type.kind = TypeKind.Primitive(Primitive.Number)
      provenance = None }

  let env =
    { Env.values = Map([ ("foo", (t, false)) ])
      Env.schemes = Map([])
      Env.isAsync = false
      Env.nonGeneric = Set([]) }

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

      Assert.Value(env, "foo", "5")
      Assert.Value(env, "bar", "\"hello\"")
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

      Assert.Value(env, "sum", "number")
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

      Assert.Value(env, "add", "fn (x: number, y: number) -> number")
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

      Assert.Value(env, "add", "fn (x: number, y: number) -> number")
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
          let bar = foo(5, "hello")
          """

      let! env = infer_script src

      Assert.Value(env, "foo", "fn (x: number, y: string) -> string | number")
      Assert.Value(env, "bar", "string | number")
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
          let bar = foo(5)
          let baz = foo("hello")
          """

      let! env = infer_script src

      Assert.Value(env, "foo", "fn <A>(x: A) -> A")
      Assert.Value(env, "bar", "5")
      Assert.Value(env, "baz", "\"hello\"")
    }

  Assert.False(Result.isError result)

[<Fact>]
let InferFuncGenericFuncWithExplicitTypeParams () =
  let result =
    result {
      let src =
        """
          let foo = fn <T>(x: T) -> T {
            return x
          }
          let bar = foo(5)
          let baz = foo("hello")
          """

      let! env = infer_script src

      Assert.Value(env, "foo", "fn <T>(x: T) -> T")
      Assert.Value(env, "bar", "5")
      Assert.Value(env, "baz", "\"hello\"")
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

      Assert.Type(env, "A", "number")
      Assert.Type(env, "B", "[string, boolean]")
      Assert.Type(env, "C", "5 | \"hello\"")
      Assert.Type(env, "D", "fn (x: number) -> number")
    }

  Assert.False(Result.isError result)

[<Fact>]
let InferLambda () =
  let result =
    result {
      let src = "let add = fn (x, y) => x + y"

      let! env = infer_script src

      Assert.Value(env, "add", "fn (x: number, y: number) -> number")
    }

  Assert.False(Result.isError result)

[<Fact(Skip = "fix this test")>]
let InferSKK () =
  let result =
    result {
      let src =
        """
          let S = fn (f) => fn (g) => fn (x) => f(x)(g(x))
          let K = fn (x) => fn (y) => x
          let I = S(K)(K)
        """

      let! env = infer_script src

      Assert.Value(
        env,
        "S",
        "fn <A, E, C, D, B, F>(f: fn (arg0: A) -> fn (arg0: B) -> C) -> fn (g: fn (arg0: A) -> B) -> fn (x: A) -> C"
      )

      Assert.Value(env, "K", "fn <A, B>(x: A) -> fn (y: B) -> A")
      // This should be `fn <A>(x: A) -> A`
      Assert.Value(env, "I", "fn <A, B>(x: A) -> B")
    }

  printfn "result = %A" result
  Assert.False(Result.isError result)
