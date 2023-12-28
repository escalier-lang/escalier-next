module ArrayTuple

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
let InferTupleLength () =
  let res =
    result {
      let src =
        """
        let tuple = [5, "hello", true]
        let length = tuple.length
        """

      let! _, env = inferScript src

      Assert.Value(env, "length", "3")
    }

  printfn "res = %A" res
  Assert.False(Result.isError res)

[<Fact>]
let InferArrayLength () =
  let res =
    result {
      let src =
        """
        let tuple: number[] = [1, 2, 3]
        let length = tuple.length
        """

      let! _, env = inferScript src

      Assert.Value(env, "length", "number")
    }

  printfn "res = %A" res
  Assert.False(Result.isError res)

[<Fact>]
let InferTupleIndexing () =
  let res =
    result {
      let src =
        """
        let tuple = [5, "hello", true]
        let first = tuple[0]
        let second = tuple[1]
        let third = tuple[2]
        let fourth = tuple[3]
        """

      let! _, env = inferScript src

      Assert.Value(env, "first", "5")
      Assert.Value(env, "second", "\"hello\"")
      Assert.Value(env, "third", "true")
      Assert.Value(env, "fourth", "undefined")
    }

  printfn "res = %A" res
  Assert.False(Result.isError res)

[<Fact>]
let InferArrayIndexing () =
  let res =
    result {
      let src =
        """
        let tuple: number[] = [1, 2, 3]
        let first = tuple[0]
        let second = tuple[1]
        let third = tuple[2]
        let fourth = tuple[3]
        """

      let! _, env = inferScript src

      Assert.Value(env, "first", "number | undefined")
      Assert.Value(env, "fourth", "number | undefined")
    }

  printfn "res = %A" res
  Assert.False(Result.isError res)

[<Fact>]
let InferForIn () =
  let res =
    result {
      let src =
        """
        for x in [1, 2, 3] {
          let y: number = x
        }
        let array: number[] = [1, 2, 3]
        for x in array {
          let y: number = x
        }
        """

      let! _ = inferScript src

      ()
    }

  printfn "res = %A" res
  Assert.False(Result.isError res)
