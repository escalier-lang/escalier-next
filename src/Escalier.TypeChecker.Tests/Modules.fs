module Modules


open FsToolkit.ErrorHandling
open System.IO.Abstractions.TestingHelpers
open Xunit

open Escalier.Parser
open Escalier.TypeChecker
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


let inferModule src =
  result {
    let! ast = Parser.parseModule src |> Result.mapError CompileError.ParseError

    let mockFileSystem = MockFileSystem()
    let! ctx, env = Prelude.getEnvAndCtx mockFileSystem "/" "/input.esc"

    let! env =
      inferModule ctx env "input.esc" ast
      |> Result.mapError CompileError.TypeError

    return ctx, env
  }

[<Fact>]
let InferBasicModule () =
  let res =
    result {
      let src =
        """
        let add = fn(a, b) => a + b
        let sub = fn(a, b) => a - b
        """

      let! _, env = inferModule src

      Assert.Value(env, "add", "fn <A: number, B: number>(a: A, b: B) -> A + B")
      Assert.Value(env, "sub", "fn <A: number, B: number>(a: A, b: B) -> A - B")
    }

  Assert.False(Result.isError res)

[<Fact>]
let InferMutuallyRecursiveFunctions () =
  let res =
    result {
      let src =
        """
        let foo = fn() => bar() + 1
        let bar = fn() => foo() - 1
        """

      let! _, env = inferModule src

      Assert.Value(env, "foo", "fn () -> number")
      Assert.Value(env, "bar", "fn () -> number")
    }

  printfn "res = %A" res
  Assert.False(Result.isError res)

[<Fact>]
let InferMutualRecursion () =
  let res =
    result {
      let src =
        """
        let even = fn (x) => if (x == 0) {
            true
        } else {
            odd(x - 1)
        }

        let odd = fn (x) => if (x == 1) {
            true
        } else {
            even(x - 1)
        }
        """

      let! _, env = inferModule src

      Assert.Value(env, "even", "fn (x: number) -> true | true")
      Assert.Value(env, "odd", "fn (x: number) -> true | true | true")
    }

  printfn "res = %A" res
  Assert.False(Result.isError res)
