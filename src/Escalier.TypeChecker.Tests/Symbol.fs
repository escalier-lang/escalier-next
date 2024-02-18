module Symbol

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
    let mockFileSystem = MockFileSystem()
    let! ctx, env = Prelude.getEnvAndCtx false mockFileSystem "/"

    let prelude =
      """
        declare let Symbol: {
          asyncIterator: unique symbol,
          iterator: unique symbol,
          match: unique symbol,
          matchAll: unique symbol,
          replace: unique symbol,
          search: unique symbol,
          species: unique symbol,
          split: unique symbol,
          toPrimitive: unique symbol,
          toStringTag: unique symbol,
        }
        type Iterator<T> = {
          next: fn () -> { done: boolean, value: T }
        }
        type Array<T> = {
          [Symbol.iterator]: fn () -> Iterator<T>
        }
        type RangeIterator<Min: number, Max: number> = {
          next: fn () -> { done: boolean, value: Min..Max }
        }
        """

    let! ast =
      Parser.parseScript prelude |> Result.mapError CompileError.ParseError

    let! env =
      inferScript ctx env "/prelude.esc" ast
      |> Result.mapError CompileError.TypeError

    let! ast = Parser.parseScript src |> Result.mapError CompileError.ParseError

    let! env =
      inferScript ctx env "input.esc" ast
      |> Result.mapError CompileError.TypeError

    return ctx, env
  }

[<Fact>]
let InfersTypeofWellknownSymbol () =
  let res =
    result {
      let src =
        """
        let iterator: typeof Symbol.iterator = Symbol.iterator
        """

      let! _, env = inferScript src

      Assert.Value(env, "iterator", "symbol()")
    }

  Assert.False(Result.isError res)

[<Fact>]
let InfersSymbolsAreUnique () =
  let res =
    result {
      let src =
        """
        let iterator: Symbol.match = Symbol.iterator
        """

      let! _, _ = inferScript src

      ()
    }

  Assert.True(Result.isError res)
