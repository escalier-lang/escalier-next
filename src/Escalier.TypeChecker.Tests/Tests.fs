module Tests

open FParsec
open FsToolkit.ErrorHandling
open Xunit

open Escalier.Data.Type
open Escalier.Parser.Parser
open Escalier.TypeChecker
open Escalier.TypeChecker.Env
open Escalier.TypeChecker.Error
open Escalier.TypeChecker.Infer

type Assert with

  static member inline Value(env: Env, name: string, expected: string) =
    let t, _ = Map.find name env.Values
    Assert.Equal(expected, t.ToString())

  static member inline Type(env: Env, name: string, expected: string) =
    let scheme = Map.find name env.Schemes
    Assert.Equal(expected, scheme.ToString())

type CompileError =
  | ParseError of ParserError
  | TypeError of TypeError

let infer src =
  result {
    let! ast =
      match run expr src with
      | Success(value, _, _) -> Result.Ok(value)
      | Failure(_s, parserError, _unit) ->
        Result.mapError CompileError.ParseError (Result.Error(parserError))

    let env = Prelude.getEnv ()

    let! t = Result.mapError CompileError.TypeError (inferExpr env ast)

    return t
  }

let inferScript src =
  result {
    let! ast =
      match run (many stmt) src with
      | Success(value, _, _) -> Result.Ok(value)
      | Failure(_s, parserError, _unit) ->
        Result.mapError CompileError.ParseError (Result.Error(parserError))

    let env = Prelude.getEnv ()

    let! env = Result.mapError CompileError.TypeError (inferScript env ast)

    return env
  }

let inferWithEnv src env =
  result {
    let! ast =
      match run expr src with
      | Success(value, _, _) -> Result.Ok(value)
      | Failure(_s, parserError, _unit) ->
        Result.mapError CompileError.ParseError (Result.Error(parserError))

    let! t = Result.mapError CompileError.TypeError (inferExpr env ast)

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

      let! eq = infer "5 == 10"
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

  let result =
    result {
      let! t = inferWithEnv "foo" env
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
      let src =
        """
        let a: number = 5
        let [b, c]: [string, boolean] = ["hello", true]
        type Point = {x: number, y: number}
        let {x, y}: Point = {x: 5, y: 10}
        """

      let! env = inferScript src

      Assert.Value(env, "a", "number")
      Assert.Value(env, "b", "string")
      Assert.Value(env, "c", "boolean")
      Assert.Value(env, "x", "number")
      Assert.Value(env, "y", "number")
    }

  Assert.False(Result.isError result)

[<Fact>]
let InferObjectDestructuring () =
  let result =
    result {
      let src =
        """
        type Point = {x: number, y: number}
        let {x, y}: Point = {x: 5, y: 10}
        let p: Point = {x, y}
        let foo = fn ({x, y}: Point) => x + y
        let sum = foo({x: 5, y: 10})
        foo({x, y})
        """

      let! env = inferScript src

      Assert.Value(env, "x", "number")
      Assert.Value(env, "y", "number")
      Assert.Value(env, "p", "Point")
      Assert.Value(env, "sum", "number")
    }

  Assert.False(Result.isError result)

[<Fact>]
let InferObjectRestSpread () =
  let result =
    result {
      let src =
        """
        let obj1 = {a: 5, b: "hello", c: true}
        let {a, ...rest} = obj1
        let obj2 = {a, ...rest}
        let foo = fn({a, ...rest}: {a: number, b: string, c: boolean}) => a
        foo(obj2)
        """

      let! env = inferScript src

      Assert.Value(env, "a", "5")
      Assert.Value(env, "rest", "{b: \"hello\", c: true}")
      Assert.Value(env, "obj2", "{a: 5} & {b: \"hello\", c: true}")
    }

  Assert.False(Result.isError result)

[<Fact>]
let InferObjProps () =
  let result =
    result {
      let src =
        """
        let obj = {a: {b: 5, c: "hello"}}
        let b = obj.a.b
        let c = obj.a.c
        """

      let! env = inferScript src

      Assert.Value(env, "b", "5")
      Assert.Value(env, "c", "\"hello\"")
    }

  Assert.False(Result.isError result)

[<Fact>]
let InferOptionalChaining () =
  let result =
    result {
      let src =
        """
        type Obj = {a?: {b?: {c: number}}}
        let obj: Obj = {a: {b: undefined}}
        let a = obj.a
        let b = obj.a?.b
        let c = obj.a?.b?.c
        type Point = {x: number, y: number}
        let p: Point = {x: 5, y: 10}
        let x = p?.x
        """

      let! env = inferScript src

      Assert.Value(env, "obj", "Obj")
      Assert.Value(env, "a", "{b?: {c: number}} | undefined")
      Assert.Value(env, "b", "{c: number} | undefined")
      Assert.Value(env, "c", "number | undefined")
      Assert.Value(env, "p", "Point")
      Assert.Value(env, "x", "number")
    }

  printf "result = %A" result
  Assert.False(Result.isError result)

[<Fact>]
let InferFactorial () =
  let result =
    result {

      let src =
        """
        let factorial = fn (n) =>
          if (n == 0) { 1 } else { n * factorial(n - 1) } 
        """

      let! env = inferScript src

      // TODO: remove <A> which is coming from the `throw` type variable
      // TODO: figure out how to get the param name back
      Assert.Value(env, "factorial", "fn <A>(arg0: number) -> number")
    }

  printf "result = %A" result
  Assert.False(Result.isError result)

[<Fact>]
let InferFuncGeneralization () =
  let result =
    result {
      let src = "let fst = fn (x, y) => x"

      let! env = inferScript src

      Assert.Value(env, "fst", "fn <A, B>(x: A, y: B) -> A")
    }

  printf "result = %A" result
  Assert.False(Result.isError result)

[<Fact(Skip = "TODO: unify tuples and arrays")>]
let InferTuple () =
  let result =
    result {
      let src =
        """
        let foo = [1, 2, 3]
        let bar = fn(nums: number[]) => nums
        let baz = bar(foo)
        """

      let! env = inferScript src

      Assert.Value(env, "foo", "[1, 2, 3]")
      Assert.Value(env, "bar", "fn (nums: number[]) -> number[]")
      Assert.Value(env, "baz", "number[]")
    }

  printfn "result = %A" result
  Assert.False(Result.isError result)
