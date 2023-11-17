module TypeCheckerTests

open Xunit
open Escalier.HindleyMilner.Parser
open Escalier.HindleyMilner.Type
open Escalier.HindleyMilner.Errors
open Escalier.HindleyMilner.TypeChecker
open Escalier.HindleyMilner
open FsToolkit.ErrorHandling
open FParsec

type Assert with

  static member inline Value(env: Env, name: string, expected: string) =
    let (t, _) = Map.find name env.Values
    Assert.Equal(expected, t.ToString())

  static member inline Type(env: Env, name: string, expected: string) =
    let scheme = Map.find name env.Schemes
    Assert.Equal(expected, scheme.ToString())

type CompileError =
  | ParseError of ParserError
  | TypeError of TypeError

let makeParam (name: string) (ty: Type) : FuncParam =
  { Pattern = Pattern.Identifier name
    Type = ty
    Optional = false }

let getEnv () =
  let arithemtic =
    (makeFunctionType
      None
      [ makeParam "left" numType; makeParam "right" numType ]
      numType,
     false)

  let comparison =
    (makeFunctionType
      None
      [ makeParam "left" numType; makeParam "right" numType ]
      boolType,
     false)

  let logical =
    (makeFunctionType
      None
      [ makeParam "left" boolType; makeParam "right" boolType ]
      boolType,
     false)

  let typeRefA =
    { Kind = makePrimitiveKind "A"
      Provenance = None }

  let typeRefB =
    { Kind = makePrimitiveKind "B"
      Provenance = None }

  let typeParams: list<TypeParam> =
    [ { Name = "A"
        Constraint = None
        Default = None }
      { Name = "B"
        Constraint = None
        Default = None } ]

  let equality =
    (makeFunctionType
      (Some(typeParams))
      [ makeParam "left" typeRefA; makeParam "right" typeRefB ]
      boolType,
     false)

  { Env.Values =
      Map.ofList
        [ ("+", arithemtic)
          ("-", arithemtic)
          ("*", arithemtic)
          ("/", arithemtic)
          ("%", arithemtic)
          ("**", arithemtic)
          ("<", comparison)
          ("<=", comparison)
          (">", comparison)
          (">=", comparison)
          ("==", equality)
          ("!=", equality)
          ("||", logical)
          ("&&", logical) ]
    Env.Schemes = Map([])
    Env.IsAsync = false }

let infer src =
  result {
    let! ast =
      match run expr src with
      | Success(value, _, _) -> Result.Ok(value)
      | Failure(s, parserError, unit) ->
        Result.mapError (CompileError.ParseError) (Result.Error(parserError))

    let env = getEnv ()
    let nonGeneric = Set([])

    let! t =
      Result.mapError (CompileError.TypeError) (inferExpr ast env nonGeneric)

    return t
  }

let inferScript src =
  result {
    let! ast =
      match run (many stmt) src with
      | Success(value, _, _) -> Result.Ok(value)
      | Failure(s, parserError, unit) ->
        Result.mapError (CompileError.ParseError) (Result.Error(parserError))

    let env = getEnv ()

    let! env = Result.mapError (CompileError.TypeError) (inferScript ast env)

    return env
  }

let inferWithEnv src env nonGeneric =
  result {
    let! ast =
      match run expr src with
      | Success(value, _, _) -> Result.Ok(value)
      | Failure(s, parserError, unit) ->
        Result.mapError (CompileError.ParseError) (Result.Error(parserError))

    let! t =
      Result.mapError (CompileError.TypeError) (inferExpr ast env nonGeneric)

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

  printfn "result = %A" result
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

      let! env = inferScript src

      Assert.Value(env, "foo", "5 | \"hello\" | true")
    }

  Assert.False(Result.isError result)

[<Fact>]
let InferIdentifier () =
  let t: Type =
    { Type.Kind = makePrimitiveKind "number"
      Provenance = None }

  let env =
    { Env.Values = Map([ ("foo", (t, false)) ])
      Env.Schemes = Map([])
      Env.IsAsync = false }

  let nonGeneric = Set([])

  let result =
    result {
      let! t = inferWithEnv "foo" env nonGeneric
      Assert.Equal("number", t.ToString())
    }

  Assert.False(Result.isError result)

[<Fact>]
let InferLetStatements () =
  let result =
    result {
      let! env = inferScript "let foo = 5\nlet bar =\"hello\""

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

      let! env = inferScript src

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

      let! env = inferScript src

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

      let! env = inferScript src

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

      let! env = inferScript src

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

      let! env = inferScript src

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

      let! env = inferScript src

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
          type Nullable<T> = T | undefined
          """

      let! env = inferScript src

      Assert.Type(env, "A", "number")
      Assert.Type(env, "B", "[string, boolean]")
      Assert.Type(env, "C", "5 | \"hello\"")
      Assert.Type(env, "D", "fn (x: number) -> number")
      Assert.Type(env, "Nullable", "<T>(T | undefined)")
    }

  Assert.False(Result.isError result)

[<Fact>]
let InferLambda () =
  let result =
    result {
      let src = "let add = fn (x, y) => x + y"

      let! env = inferScript src

      Assert.Value(env, "add", "fn (x: number, y: number) -> number")
    }

  Assert.False(Result.isError result)

[<Fact>]
let InferSKK () =
  let result =
    result {
      let src =
        """
          let S = fn (f) => fn (g) => fn (x) => f(x)(g(x))
          let K = fn (x) => fn (y) => x
          let I = S(K)(K)
        """

      let! env = inferScript src

      Assert.Value(
        env,
        "S",
        "fn <A, C, B>(f: fn (arg0: A) -> fn (arg0: B) -> C) -> fn (g: fn (arg0: A) -> B) -> fn (x: A) -> C"
      )

      Assert.Value(env, "K", "fn <A, B>(x: A) -> fn (y: B) -> A")
      Assert.Value(env, "I", "fn <A>(x: A) -> A")
    }

  Assert.False(Result.isError result)

[<Fact>]
let InferTypeAnn () =
  let result =
    result {
      let src = "let x: number = 5"

      let! env = inferScript src

      Assert.Value(env, "x", "number")
    }

  Assert.False(Result.isError result)
