module Tests

open FParsec
open FsToolkit.ErrorHandling
open System.IO.Abstractions.TestingHelpers
open Xunit

open Escalier.Data.Type
open Escalier.Parser
open Escalier.TypeChecker
open Escalier.TypeChecker.Prune
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

type CompileError = Prelude.CompileError

let infer src =
  result {
    let! ast =
      match run Parser.expr src with
      | Success(value, _, _) -> Result.Ok(value)
      | Failure(_s, parserError, _unit) ->
        Result.mapError CompileError.ParseError (Result.Error(parserError))

    let mockFileSystem = MockFileSystem()
    let! ctx, env = Prelude.getEnvAndCtx mockFileSystem "/" "/input.esc"

    let! t = Result.mapError CompileError.TypeError (inferExpr ctx env ast)

    return simplify t
  }

let inferScript src =
  result {
    let! ast = Parser.parseScript src |> Result.mapError CompileError.ParseError

    let mockFileSystem = MockFileSystem()
    let! ctx, env = Prelude.getEnvAndCtx mockFileSystem "/" "/input.esc"

    let! env =
      inferScript ctx env "input.esc" ast
      |> Result.mapError CompileError.TypeError

    return ctx, env
  }

let inferWithEnv src env =
  result {
    let! ast =
      match run Parser.expr src with
      | Success(value, _, _) -> Result.Ok(value)
      | Failure(_s, parserError, _unit) ->
        Result.mapError CompileError.ParseError (Result.Error(parserError))

    let ctx =
      Ctx((fun ctx filename import -> env), (fun ctx filename import -> ""))

    let! t = Result.mapError CompileError.TypeError (inferExpr ctx env ast)

    return t
  }

let printDiagnostic (d: Diagnostic) =
  let rec printReasons (rs: list<TypeError>) =
    match rs with
    | [] -> ()
    | r :: rs ->
      printReason r
      printReasons rs

  and printReason (r: TypeError) =
    match r with
    | NotImplemented s -> printf "- Not implemented: %s\n" s
    | SemanticError s -> printf "- Semantic error: %s\n" s
    | NotInferred -> printf "- Type could not be inferred\n"
    | TypeMismatch(t1, t2) -> printf $"- Type mismatch: {t1} and {t2}\n"
    // printfn "t1.Provenance = %A" (prune t1).Provenance
    // printfn "t2.Provenance = %A" (prune t2).Provenance
    // printfn "t2 = %A" t2
    | RecursiveUnification(t1, t2) ->
      printf "- Recursive unification: {t1} and {t2}\n"
    | WrongNumberOfTypeArgs -> printf "- Wrong number of type arguments\n"

  printf "ERROR: %s\n" d.Description

  printReasons d.Reasons

let rec printDiagnostics (ds: list<Diagnostic>) =
  match ds with
  | [] -> ()
  | d :: ds ->
    printDiagnostic d
    printDiagnostics ds


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
let InferSimpleTypeError () =
  let result =
    result {
      let src =
        """
        let y: string = "hello"
        let x: number = y
        """

      let! ctx, env = inferScript src

      printDiagnostics ctx.Diagnostics
    }

  printfn "result = %A" result
  Assert.True(Result.isError result)

[<Fact>]
let InferBinaryOperators () =
  let result =
    result {
      let! sum = infer "5 + 10"
      Assert.Equal("15", sum.ToString())

      let! str = infer "\"Hello, \" ++ \"world!\""
      Assert.Equal("\"Hello, world!\"", str.ToString())

      let! lt = infer "5 < 10"
      Assert.Equal("true", lt.ToString())

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

      let! _, env = inferScript src

      Assert.Value(env, "foo", "5 | \"hello\" | true")
    }

  Assert.False(Result.isError result)

[<Fact>]
let InferIdentifier () =
  let t: Type =
    { Kind = makeTypeRefKind "number"
      Provenance = None }

  let env =
    { Env.BinaryOps = Map.empty
      Env.UnaryOps = Map.empty
      Env.Values = Map([ ("foo", (t, false)) ])
      Env.Schemes = Map([])
      Env.IsAsync = false
      Env.IsPatternMatching = false }

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
      let! _, env = inferScript "let foo = 5\nlet bar =\"hello\""

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

      let! _, env = inferScript src

      Assert.Value(env, "sum", "15")
    }

  Assert.False(Result.isError result)

[<Fact>]
let InferFuncParams () =
  let result =
    result {
      let src =
        """
          let addNums = fn (x, y) {
            return x + y
          }
          let addStrs = fn (x, y) {
            return x ++ y
          }
          let msg = addStrs("Hello, ", "world!")
          let greeting: string = "Bonjour, "
          let frMsg = addStrs(greeting, "monde!")
          """

      let! _, env = inferScript src

      Assert.Value(
        env,
        "addNums",
        "fn <A: number, B: number>(x: A, y: B) -> A + B"
      )

      Assert.Value(
        env,
        "addStrs",
        "fn <A: string, B: string>(x: A, y: B) -> A ++ B"
      )

      Assert.Value(env, "msg", "\"Hello, world!\"")
      Assert.Value(env, "frMsg", "string")
    }

  Assert.False(Result.isError result)

[<Fact>]
let InferBinaryOpStressTest () =
  let result =
    result {
      let src =
        """
          let foo = fn (a, b, c) {
            return a * b + c
          }
          let double = fn (x) {
            return 2 * x
          }
          let inc = fn (x) {
            return x + 1
          }
          let bar = foo(double(1), inc(2), 3)
          let baz: number = 5
          let qux = double(baz)
          """

      let! _, env = inferScript src

      Assert.Value(
        env,
        "foo",
        "fn <C: number, A: number, B: number>(a: A, b: B, c: C) -> A * B + C"
      )

      Assert.Value(env, "double", "fn <A: number>(x: A) -> 2 * A")
      Assert.Value(env, "inc", "fn <A: number>(x: A) -> A + 1")
      Assert.Value(env, "bar", "9")
      Assert.Value(env, "qux", "number")
    }

  printfn "result = %A" result
  Assert.False(Result.isError result)

[<Fact>]
let InferFuncParamsWithTypeAnns () =
  let result =
    result {
      let src =
        """
          let addNums = fn (x: number, y: number) -> number {
            return x + y
          }
          let addStrs = fn (x: string, y: string) -> string {
            return x ++ y
          }
          """

      let! _, env = inferScript src

      Assert.Value(env, "addNums", "fn (x: number, y: number) -> number")
      Assert.Value(env, "addStrs", "fn (x: string, y: string) -> string")
    }

  printfn "result = %A" result
  Assert.False(Result.isError result)

[<Fact>]
let InferFuncWithMultipleReturns () =
  let result =
    result {
      let src =
        """
          let foo = fn <A: number>(x: A, y: string) {
            if (x > 0) {
              return x
            }
            return y
          }
          let bar = foo(5, "hello")
          """

      let! _, env = inferScript src

      Assert.Value(env, "foo", "fn <A: number>(x: A, y: string) -> string | A")
      Assert.Value(env, "bar", "string | 5")
    }

  printfn "result = %A" result
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

      let! _, env = inferScript src

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

      let! _, env = inferScript src

      Assert.Value(env, "foo", "fn <T>(x: T) -> T")
      Assert.Value(env, "bar", "5")
      Assert.Value(env, "baz", "\"hello\"")
    }

  printfn "result = %A" result
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

      let! _, env = inferScript src

      Assert.Type(env, "A", "number")
      Assert.Type(env, "B", "[string, boolean]")
      Assert.Type(env, "C", "5 | \"hello\"")
      Assert.Type(env, "D", "fn (x: number) -> number")
      Assert.Type(env, "Nullable", "<T>(T | undefined)")
    }

  printfn "result = %A" result

  Assert.False(Result.isError result)


[<Fact>]
let InferPrivateDecl () =
  let result =
    result {
      let src =
        """
          let makePoint = fn (x, y) {
            type Point = {x: number, y: number}
            let point: Point = {x, y}
            return point
          }
          let p = makePoint(5, 10)
          let {x, y} = p
          """

      let! _, env = inferScript src

      Assert.Value(env, "makePoint", "fn (x: number, y: number) -> Point")
      Assert.Value(env, "p", "Point")
      Assert.Value(env, "x", "number")
      Assert.Value(env, "y", "number")
    }

  printfn "result = %A" result
  Assert.False(Result.isError result)

[<Fact>]
let InferTypeAliasOfPrimtiveType () =
  let result =
    result {
      let src =
        """
        type Bar = number
        let x: Bar = 5
        """

      let! _, env = inferScript src

      Assert.Value(env, "x", "Bar")
    }

  printfn "result = %A" result
  Assert.False(Result.isError result)

[<Fact>]
let InferTypeAliasOfTypeParam () =
  let result =
    result {
      let src =
        """
        let foo = fn <A>(x: A) {
          type B = A
          let y: B = x
          return y
        }
        let bar = fn <A>(x: A) -> A {
          type B = A
          let y: B = x
          return y
        }
        let z = foo(5)
        let w: number = z
        """

      let! _, env = inferScript src

      Assert.Value(env, "foo", "fn <A>(x: A) -> B")
      Assert.Value(env, "bar", "fn <A>(x: A) -> A")
      Assert.Value(env, "z", "B")
    }

  printfn "result = %A" result
  Assert.False(Result.isError result)


[<Fact>]
let InferLambda () =
  let result =
    result {
      let src = "let add = fn (x, y) => x + y"

      let! _, env = inferScript src

      Assert.Value(env, "add", "fn <A: number, B: number>(x: A, y: B) -> A + B")
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

      let! _, env = inferScript src

      Assert.Value(
        env,
        "S",
        "fn <A, B, C>(f: fn (arg0: A) -> fn (arg0: B) -> C) -> fn (g: fn (arg0: A) -> B) -> fn (x: A) -> C"
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

      let! _, env = inferScript src

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

      let! _, env = inferScript src

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

      let! _, env = inferScript src

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

      let! _, env = inferScript src

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

      let! _, env = inferScript src

      Assert.Value(env, "obj", "Obj")
      Assert.Value(env, "a", "{b?: {c: number}} | undefined")
      Assert.Value(env, "b", "{c: number} | undefined")
      Assert.Value(env, "c", "number | undefined")
      Assert.Value(env, "p", "Point")
      Assert.Value(env, "x", "number")
    }

  printfn "result = %A" result
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

      let! _, env = inferScript src

      // TODO: figure out how to get the param name back
      Assert.Value(env, "factorial", "fn (arg0: number) -> number")
    }

  printf "result = %A" result
  Assert.False(Result.isError result)

[<Fact>]
let InferRecursive () =
  let result =
    result {

      let src =
        """
        let rec = fn () => rec()
        """

      let! _, env = inferScript src

      // TODO: figure out how to get the param name back
      Assert.Value(env, "rec", "fn <A>() -> A")
    }

  printf "result = %A" result
  Assert.False(Result.isError result)

[<Fact>]
let InferRecursiveSequence () =
  let result =
    result {

      let src =
        """
        let seq = fn () => seq() + 1
        """

      let! _, env = inferScript src

      // TODO: figure out how to get the param name back
      Assert.Value(env, "seq", "fn () -> number")
    }

  printf "result = %A" result
  Assert.False(Result.isError result)

[<Fact(Skip = "TODO: handle recursive types")>]
let InferRecursiveType () =
  let result =
    result {

      let src =
        """
        type Foo = number | Foo[]
        """

      let! _, env = inferScript src

      Assert.Type(env, "Foo", "number | Foo[]")
    }

  printf "result = %A" result
  Assert.False(Result.isError result)

[<Fact>]
let InferFuncGeneralization () =
  let result =
    result {
      let src = "let fst = fn (x, y) => x"

      let! _, env = inferScript src

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

      let! _, env = inferScript src

      Assert.Value(env, "foo", "[1, 2, 3]")
      Assert.Value(env, "bar", "fn (nums: number[]) -> number[]")
      Assert.Value(env, "baz", "number[]")
    }

  printfn "result = %A" result
  Assert.False(Result.isError result)

[<Fact>]
let InferCallFuncWithSingleWrongArg () =
  let result =
    result {
      let src =
        """
        let add = fn (x, y) => {value: x + y}
        let sum = add("hello", "world")
        """

      let! ctx, env = inferScript src

      printDiagnostics ctx.Diagnostics
      // TODO: simplify all binary types inside a complex type
      Assert.Value(env, "sum", "{value: number + number}")
    }

  Assert.False(Result.isError result)

[<Fact>]
let InferCallFuncWithWrongArgs () =
  let result =
    result {
      let src =
        """
        let add = fn (x, y) => x + y
        let sum = "hello" + "world"
        """

      let! ctx, env = inferScript src

      printDiagnostics ctx.Diagnostics
      Assert.Value(env, "sum", "number")
    }

  Assert.False(Result.isError result)

[<Fact>]
let InferCallGenericFuncWithWrongArg () =
  let result =
    result {
      let src =
        """
        let foo = fn <T: number>(x: T) -> T => x
        let bar = foo("bar")
        """

      let! ctx, env = inferScript src

      printDiagnostics ctx.Diagnostics
      Assert.Value(env, "bar", "number")
    }

  Assert.False(Result.isError result)

[<Fact>]
let InferCallGenericFuncWithComplexReturnAndWrongArg () =
  let result =
    result {
      let src =
        """
        let foo = fn <T: number>(x: T) => {value: x}
        let bar = foo("hello")
        """

      let! ctx, env = inferScript src

      printDiagnostics ctx.Diagnostics
      Assert.Value(env, "bar", "{value: number}")
    }

  Assert.False(Result.isError result)

[<Fact>]
let InferFuncTypeAnnotation () =
  let result =
    result {
      let src =
        """
        let foo: fn <T: number>(x: T) -> T = fn (x) => x
        let x = foo(5)
        let y = foo("hello")
        """

      let! ctx, env = inferScript src

      printDiagnostics ctx.Diagnostics
      Assert.Value(env, "foo", "fn <T: number>(x: T) -> T")
      Assert.Value(env, "x", "5")
      Assert.Value(env, "y", "number")
    }

  Assert.False(Result.isError result)

[<Fact>]
let PassingTooManyArgsIsOkay () =
  let result =
    result {
      let src =
        """
        let add = fn (x, y) => x + y
        let sum = add(5, 10, "hello")
        """

      let! _, env = inferScript src

      Assert.Value(env, "sum", "15")
    }

  Assert.False(Result.isError result)

[<Fact>]
let PassingTooFewArgsIsAnError () =
  let result =
    result {
      let src =
        """
        let add = fn (x, y) => x + y
        let sum = add(5)
        """

      let! _, _ = inferScript src

      ()
    }

  printfn "result = %A" result
  Assert.True(Result.isError result)

[<Fact>]
let InferDeclare () =
  let result =
    result {
      let src = "declare let [x, y]: [number, string, boolean]"
      let! _, env = inferScript src
      Assert.Value(env, "x", "number")
      Assert.Value(env, "y", "string")
    }

  printfn "result = %A" result

  Assert.False(Result.isError result)

[<Fact>]
let InferTemplateLiteralType () =
  let result =
    result {

      let src =
        """
        type Foo = `foo${number}`
        let x: Foo = "foo123"
        type Bar = `A${string}B`
        let y: Bar = "A1B"
        type Baz = `A${string}B${string}C`
        let z: Baz = "A1B2C"
        let w: Baz = "ABCBC"
        """

      let! _, env = inferScript src
      Assert.Value(env, "x", "Foo")
      Assert.Value(env, "y", "Bar")
      Assert.Value(env, "z", "Baz")
      Assert.Value(env, "w", "Baz")
    }

  printfn "result = %A" result

  Assert.False(Result.isError result)

[<Fact>]
let InferTemplateLiteralTypeWithUnions () =
  let result =
    result {

      let src =
        """
        type Dir = `${"top" | "bottom"}-${"left" | "right"}`
        let a: Dir = "top-left"
        let b: Dir = "top-right"
        let c: Dir = "bottom-right"
        let d: Dir = "bottom-left"
        """

      let! _, env = inferScript src
      Assert.Value(env, "a", "Dir")
      Assert.Value(env, "b", "Dir")
      Assert.Value(env, "c", "Dir")
      Assert.Value(env, "d", "Dir")
    }

  Assert.False(Result.isError result)

[<Fact>]
let ParseTemplateLiteralType () =
  let mutable parser: Parser<unit, unit> = eof
  parser <- (pstring "C" |>> ignore) .>> parser
  parser <- many (notFollowedBy parser >>. anyChar) |>> ignore
  parser <- (pstring "B" |>> ignore) .>> parser
  parser <- many (notFollowedBy parser >>. anyChar) |>> ignore
  parser <- (pstring "A" |>> ignore) .>> parser

  let result = run parser "ABCBC"

  match result with
  | Success(value, _, _) -> printfn "value = %A" value
  | Failure(s, _, _) -> printfn "s = %A" s

[<Fact>]
let ParseTemplateLiteralTypeWithUnions () =
  let mutable parser: Parser<unit, unit> = eof
  parser <- (pstring "right" <|> pstring "left" |>> ignore) .>> parser
  parser <- (pstring "-" |>> ignore) .>> parser
  parser <- (pstring "top" <|> pstring "bottom" |>> ignore) .>> parser

  let result = run parser "top-left"

  match result with
  | Success(value, _, _) -> printfn "value = %A" value
  | Failure(s, _, _) -> printfn "s = %A" s

[<Fact>]
let InferTemplateLiteralTypeError () =
  let result =
    result {
      let src =
        """
        type Foo = `foo${number}`
        let x: Foo = "foo123abc"
        """

      let! _, _ = inferScript src
      ()
    }

  Assert.True(Result.isError result)

[<Fact>]
let InferTemplateLiteralTypeErrorWithUnion () =
  let result =
    result {
      let src =
        """
        type Dir = `${"top" | "bottom"}-${"left" | "right"}`
        let x: Dir = "top-bottom"
        """

      let! _, _ = inferScript src
      ()
    }

  Assert.True(Result.isError result)

[<Fact>]
let InferUnaryOperations () =
  let result =
    result {

      let src =
        """
        let x = 5
        let y = -x
        let z = !x
        let w = +x
        """

      let! _, env = inferScript src
      Assert.Value(env, "x", "5")
      Assert.Value(env, "y", "-5")
      Assert.Value(env, "z", "!5")
      Assert.Value(env, "w", "+5")
    }

  Assert.False(Result.isError result)
