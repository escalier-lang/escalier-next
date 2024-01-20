module Mutability

open FParsec
open FsToolkit.ErrorHandling
open System.IO.Abstractions.TestingHelpers
open Xunit

open Escalier.Parser
open Escalier.TypeChecker
open Escalier.TypeChecker.Prune
open Escalier.TypeChecker.Env
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

[<Fact>]
let InferBasicMutability () =
  let result =
    result {
      let src =
        """
        let mut x: number = 5
        x = 10
        """

      let! _, _ = inferScript src
      ()
    }

  Assert.False(Result.isError result)

[<Fact>]
let InferBasicObjectMutability () =
  let result =
    result {
      let src =
        """
        type Point = {x: number, y: number}
        let mut p: Point = {x: 0, y: 0}
        p.x = 5
        p.y = 10
        """

      let! _, _ = inferScript src
      ()
    }

  Assert.False(Result.isError result)

[<Fact>]
let InferBasicArrayMutability () =
  let result =
    result {
      let src =
        """
        let mut array: number[] = [1, 2, 3]
        array[3] = 4
        """

      let! _, _ = inferScript src
      ()
    }

  Assert.False(Result.isError result)

[<Fact>]
let DisallowAssigningToImmutableBindings () =
  let result =
    result {
      let src =
        """
        let x: number = 5
        x = 10
        """

      let! _, _ = inferScript src
      ()
    }

  Assert.True(Result.isError result)

[<Fact>]
let InferFnWithMutableParam () =
  let result =
    result {
      let src =
        """
        let foo = fn (mut array: (number | string)[]) {
          array[0] = 5
          array[1] = "hello"
        }
        """

      let! _, env = inferScript src

      Assert.Value(
        env,
        "foo",
        "fn (mut array: (number | string)[]) -> undefined"
      )
    }

  Assert.False(Result.isError result)

[<Fact>]
let MutableParamsAreInvariant () =
  let result =
    result {
      let src =
        """
        let foo = fn (mut array: (number | string)[]) {
          array[0] = 5
          array[1] = "hello"
        }
        let numbers: (number | string)[] = [1, 2, 3]
        foo(numbers)
        """

      let! _, env = inferScript src

      Assert.Value(
        env,
        "foo",
        "fn (mut array: (number | string)[]) -> undefined"
      )
    }

  Assert.False(Result.isError result)

[<Fact>]
let MutableParamsCantBeCovariant () =
  let result =
    result {
      let src =
        """
        let foo = fn (mut array: (number | string)[]) {
          array[0] = 5
          array[1] = "hello"
        }
        let mut numbers: number[] = [1, 2, 3]
        foo(numbers)
        """

      let! ctx, _ = inferScript src

      Assert.Equal(ctx.Diagnostics.Length, 1)
    }

  Assert.False(Result.isError result)

[<Fact>]
let ImmutableAssignmentIsBeCovariant () =
  let result =
    result {
      let src =
        """
        let foo: number[] = [1, 2, 3]
        let bar: (number | string)[] = foo
        """

      let! ctx, _ = inferScript src

      Assert.Equal(ctx.Diagnostics.Length, 0)
    }

  Assert.False(Result.isError result)

[<Fact>]
let MutableAssignmentIsInvariant () =
  let result =
    result {
      let src =
        """
        let mut foo: number[] = [1, 2, 3]
        let mut bar: number[] = foo
        """

      let! _ = inferScript src
      ()
    }

  Assert.False(Result.isError result)

[<Fact>]
let MutableAssignmentCantBeCovariant () =
  let result =
    result {
      let src =
        """
        let mut foo: number[] = [1, 2, 3]
        let mut bar: (number | string)[] = foo
        """

      let! _ = inferScript src
      ()
    }

  Assert.True(Result.isError result)

[<Fact>]
let CantPassImmutableArgsToMutableParams () =
  let result =
    result {
      let src =
        """
        let foo = fn (mut array: number[]) {
          array[0] = 5
          array[1] = "hello"
        }
        let numbers: number[] = [1, 2, 3]
        foo(numbers)
        """

      let! ctx, _ = inferScript src

      Assert.Equal(ctx.Diagnostics.Length, 1)
    }

  Assert.False(Result.isError result)

[<Fact>]
let ImmutableParamsAreCovariant () =
  let result =
    result {
      let src =
        """
        let foo = fn (array: (number | string)[]) {
          return array.length
        }
        let numbers: number[] = [1, 2, 3]
        foo(numbers)
        """

      let! _, env = inferScript src

      Assert.Value(
        env,
        "foo",
        "fn (array: (number | string)[]) -> unique number"
      )
    }

  Assert.False(Result.isError result)

[<Fact>]
let MutablePartialInitialization () =
  let result =
    result {
      let src =
        """
        type Point = {x:number, y:number}
        type Line = {p0: Point, p1: Point}
        
        let mut p0: Point = {x: 0, y: 0}
        let mut p1: Point = {x: 5, y: 5}
        
        let line: Line = {p0, p1}
        let {p0: mut start, p1: mut end}: Line = {p0, p1}
        let {p0: start, p1: mut end}: Line = {p0, p1}
        let {p0: mut start, p1: end}: Line = {p0, p1}
        """

      let! _ = inferScript src
      ()
    }

  printfn "result = %A" result
  Assert.False(Result.isError result)

[<Fact>]
let MutableInitializationError () =
  let result =
    result {
      let src =
        """
        type Point = {x:number, y:number}
        type Line = {p0: Point, p1: Point}
        
        let mut p0 = {x: 0, y: 0}
        let p1 = {x: 5, y: 5}
        
        let mut line: Line = {p0, p1}
        """

      let! _ = inferScript src
      ()
    }

  Assert.True(Result.isError result)

[<Fact>]
let MutablePartialInitializationError () =
  let result =
    result {
      let src =
        """
        type Point = {x:number, y:number}
        type Line = {p0: Point, p1: Point}
        
        let mut p0 = {x: 0, y: 0}
        let p1 = {x: 5, y: 5}
        
        let {p0: start, p1: mut end}: Line = {p0, p1}
        """

      let! _ = inferScript src
      ()
    }

  Assert.True(Result.isError result)
