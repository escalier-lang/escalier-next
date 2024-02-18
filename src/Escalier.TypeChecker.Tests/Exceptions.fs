module Exceptions

open FsToolkit.ErrorHandling
open System.IO.Abstractions.TestingHelpers
open Xunit

open Escalier.Compiler
open Escalier.Parser
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


let inferScript src =
  result {
    let! ast = Parser.parseScript src |> Result.mapError CompileError.ParseError

    let mockFileSystem = MockFileSystem()
    let! ctx, env = Prelude.getEnvAndCtx false mockFileSystem "/"

    let! env =
      inferScript ctx env "input.esc" ast
      |> Result.mapError CompileError.TypeError

    return ctx, env
  }


[<Fact>]
let InfersExplicitThrow () =
  let res =
    result {
      let src =
        """
        let foo = fn (x) {
          if x < 0 {
            throw "RangeError"
          }
          return x
        }
        """

      let! _, env = inferScript src

      Assert.Value(
        env,
        "foo",
        "fn <A: number>(x: A) -> A throws \"RangeError\""
      )
    }

  printfn "res = %A" res
  Assert.False(Result.isError res)

[<Fact>]
let InfersThrowExpression () =
  let res =
    result {
      let src =
        """
        let foo = fn (x) =>
          if x < 0 { throw "RangeError" } else { x }
        """

      let! _, env = inferScript src

      Assert.Value(
        env,
        "foo",
        "fn <A: number>(x: A) -> A throws \"RangeError\""
      )
    }

  Assert.False(Result.isError res)

[<Fact>]
let InfersJustThrowExpression () =
  let res =
    result {
      let src =
        """
        let foo = fn <T: string>(exc: T) => throw exc
        """

      let! _, env = inferScript src

      Assert.Value(env, "foo", "fn <T: string>(exc: T) -> never throws T")
    }

  Assert.False(Result.isError res)

[<Fact>]
let InfersThrowingMultipleExpressions () =
  let res =
    result {
      let src =
        """
        let foo = fn <A: number>(x: A) =>
          if x < 0 { throw "RangeError" } else { throw "BoundsError" }
        """

      let! _, env = inferScript src

      Assert.Value(
        env,
        "foo",
        "fn <A: number>(x: A) -> never throws (\"BoundsError\" | \"RangeError\")"
      )
    }

  Assert.False(Result.isError res)

[<Fact>]
let InfersThrowsFromCall () =
  let res =
    result {
      let src =
        """
        let foo = fn (x) =>
          if x < 0 { throw "RangeError" } else { x }
          
        let bar = fn (x) => foo(x)
        """

      let! _, env = inferScript src

      Assert.Value(
        env,
        "foo",
        "fn <A: number>(x: A) -> A throws \"RangeError\""
      )

      Assert.Value(
        env,
        "bar",
        "fn <A: number>(x: A) -> A throws \"RangeError\""
      )
    }

  Assert.False(Result.isError res)

[<Fact>]
let InferCatchesException () =
  let res =
    result {
      let src =
        """
        let foo = fn (x) =>
          if x < 0 { throw "RangeError" } else { x }
          
        let bar = fn (x) =>
          try {
            foo(x)
          } catch {
            | "RangeError" => 0
          }
        """

      let! _, env = inferScript src

      Assert.Value(env, "bar", "fn <A: number>(x: A) -> A | 0")
    }

  Assert.False(Result.isError res)

[<Fact>]
let InferCatchesMultipleExceptions () =
  let res =
    result {
      let src =
        """
        let foo = fn <A: number>(x: A) =>
          if x < 0 { throw "RangeError" } else { throw "BoundsError" }
          
        let bar = fn (x) =>
          try {
            foo(x)
          } catch {
            | "RangeError" => 0
            | "BoundsError" => 0
          }
        """

      let! _, env = inferScript src

      Assert.Value(env, "bar", "fn <A: number>(x: A) -> 0")
    }

  printfn "res = %A" res
  Assert.False(Result.isError res)

[<Fact>]
let InferCatchesOneOfManyExceptions () =
  let res =
    result {
      let src =
        """
        let foo = fn <A: number>(x: A) =>
          if x < 0 { throw "RangeError" } else { throw "BoundsError" }
          
        let bar = fn (x) =>
          try {
            foo(x)
          } catch {
            | "RangeError" => 0
          }
        """

      let! _, env = inferScript src

      Assert.Value(
        env,
        "bar",
        "fn <A: number>(x: A) -> 0 throws \"BoundsError\""
      )
    }

  printfn "res = %A" res
  Assert.False(Result.isError res)

[<Fact>]
let InferTryFinally () =
  let res =
    result {
      let src =
        """
        let foo = fn (x) =>
          if x < 0 { throw "RangeError" } else { x }
        let cleanup = fn () => {}

        let bar = fn (x) =>
          try {
            foo(x)
          } finally {
            cleanup()
          }
        """

      let! ctx, env = inferScript src

      Assert.Value(
        env,
        "bar",
        "fn <A: number>(x: A) -> A throws \"RangeError\""
      )
    }

  Assert.False(Result.isError res)

[<Fact>]
let InferTryCatchFinally () =
  let res =
    result {
      let src =
        """
        let foo = fn (x) =>
          if x < 0 { throw "RangeError" } else { x }
        let cleanup = fn () => {}

        let bar = fn (x) =>
          try {
            foo(x)
          } catch {
            | "RangeError" => 0
          } finally {
            cleanup()
          }
        """

      let! ctx, env = inferScript src

      Assert.Value(env, "bar", "fn <A: number>(x: A) -> A | 0")
    }

  printfn "res = %A" res
  Assert.False(Result.isError res)
